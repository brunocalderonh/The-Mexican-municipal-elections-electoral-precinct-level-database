# Basic setup
rm(list = ls())          # clear all objects in memory
#dev.off()                  # reload graphic device
cat("\014")                # clear console
options(max.print = 5000)  # expand display
options(scipen=10)
# Load packages

if (!require("pacman")) install.packages("pacman")  # load packages

pacman::p_load (dplyr, haven, readxl, tidyverse, tidyr, openxlsx, data.table, 
                stringr, janitor, purrr)

# Set working directory
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, ""))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

remove_accents <- function(x) {
  x %>%
    str_replace_all("á|Á", "A") %>% str_replace_all("é|É", "E") %>%
    str_replace_all("í|Í", "I") %>% str_replace_all("ó|Ó", "O") %>%
    str_replace_all("ú|Ú", "U") %>% str_replace_all("ñ|Ñ", "N") %>%
    str_replace_all("ü|Ü", "U")
}

# Complete SLP uniqueid mapping from JOHN and SALVADOR
assign_slp_uniqueid <- function(municipality) {
  case_when(
    grepl("AHUALULCO", municipality) ~ 24001,
    grepl("ALAQUINES", municipality) ~ 24002,
    grepl("AQUISMON|AQUISMÓN", municipality) ~ 24003,
    grepl("ARMADILLO DE LOS INFANTE", municipality) ~ 24004,
    grepl("AXTLA DE TERRAZAS", municipality) ~ 24053,
    grepl("CARDENAS|CÁRDENAS", municipality) ~ 24005,
    grepl("CATORCE", municipality) ~ 24006,
    grepl("CEDRAL", municipality) ~ 24007,
    grepl("CERRITOS", municipality) ~ 24008,
    grepl("CERRO DE SAN PEDRO", municipality) ~ 24009,
    grepl("CHARCAS", municipality) ~ 24015,
    grepl("CIUDAD DEL MAIZ|CIUDAD DEL MAÍZ", municipality) ~ 24010,
    grepl("CIUDAD FERNANDEZ|CIUDAD FERNÁNDE", municipality) ~ 24011,
    grepl("CIUDAD VALLES", municipality) ~ 24013,
    grepl("COXCATLAN|COXCATLÁN", municipality) ~ 24014,
    grepl("EBANO", municipality) ~ 24016,
    grepl("EL NARANJO", municipality) ~ 24058,
    grepl("GUADALCAZAR|GUADALCÁZAR", municipality) ~ 24017,
    grepl("HUEHUETLAN|HUEHUETLÁN", municipality) ~ 24018,
    grepl("LAGUNILLAS", municipality) ~ 24019,
    grepl("MATEHUALA", municipality) ~ 24020,
    grepl("MATLAPA", municipality) ~ 24057,
    grepl("MEXQUITIC DE CARMONA", municipality) ~ 24021,
    grepl("MOCTEZUMA", municipality) ~ 24022,
    grepl("RAYON|RAYÓN", municipality) ~ 24023,
    grepl("RIOVERDE", municipality) ~ 24024,
    grepl("SALINAS", municipality) ~ 24025,
    grepl("SAN ANTONIO", municipality) ~ 24026,
    grepl("SAN CIRO DE ACOSTA", municipality) ~ 24027,
    grepl("SAN LUIS POTOSI|SAN LUIS POTOSÍ", municipality) ~ 24028,
    grepl("SAN MARTIN CHALCHICUAUTLA|SAN MARTÍN CHALCHICUAUTLA", municipality) ~ 24029,
    grepl("SAN NICOLAS TOLENTINO|SAN NICOLÁS TOLENTINO", municipality) ~ 24030,
    grepl("SAN VICENTE TANCUAYALAB", municipality) ~ 24034,
    grepl("SANTA CATARINA", municipality) ~ 24031,
    grepl("SANTA MARIA DEL RIO|SANTA MARÍA DEL RÍO", municipality) ~ 24032,
    grepl("SANTO DOMINGO", municipality) ~ 24033,
    grepl("SOLEDAD DE GRACIANO SANCHEZ|SOLEDAD DE GRACIANO SÁNCHEZ", municipality) ~ 24035,
    grepl("TAMASOPO", municipality) ~ 24036,
    grepl("TAMAZUNCHALE", municipality) ~ 24037,
    grepl("TAMPACAN|TAMPACÁN", municipality) ~ 24038,
    grepl("TAMPAMOLON CORONA|TAMPAMOLÓN CORONA", municipality) ~ 24039,
    grepl("TAMUIN|TAMUÍN", municipality) ~ 24040,
    grepl("TANCANHUITZ", municipality) ~ 24012,
    grepl("TANLAJAS|TANLAJÁS", municipality) ~ 24041,
    grepl("TANQUIAN DE ESCOBEDO|TANQUIÁN DE ESCOBEDO", municipality) ~ 24042,
    grepl("TIERRA NUEVA", municipality) ~ 24043,
    grepl("VANEGAS", municipality) ~ 24044,
    grepl("VENADO", municipality) ~ 24045,
    grepl("VILLA DE ARISTA", municipality) ~ 24056,
    grepl("VILLA DE ARRIAGA", municipality) ~ 24046,
    grepl("VILLA DE GUADALUPE", municipality) ~ 24047,
    grepl("VILLA DE LA PAZ", municipality) ~ 24048,
    grepl("VILLA DE RAMOS", municipality) ~ 24049,
    grepl("VILLA DE REYES", municipality) ~ 24050,
    grepl("VILLA HIDALGO", municipality) ~ 24051,
    grepl("VILLA JUAREZ|VILLA JUÁREZ", municipality) ~ 24052,
    grepl("XILITLA", municipality) ~ 24054,
    grepl("ZARAGOZA", municipality) ~ 24055,
    TRUE ~ NA_real_
  )
}

##############################################################################
# ============== 1) Ayu_Seccion_1997_No_LN.csv ==============================
##############################################################################

# Equivalent to: insheet using Ayu_Seccion_1997_No_LN.csv, clear
df_1997 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/1997/Ayu_Seccion_1997_No_LN.csv") 
colnames(df_1997) <- tolower(colnames(df_1997))

df_1997 <-df_1997 %>%
  rename(
    municipality = municipio,
    section      = seccion
    # listanominal = nominal  # (commented out)
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
    NPP  = npp,
    noreg = "no registrados"
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
  select(state, section, month, year, lista) %>% 
  filter(state == "SAN LUIS POTOSI")

df_1997 <- df_1997 %>%
  left_join(df_all, by = c("section")) %>%
  filter(month == "July", year == 1997) %>%
  filter(!is.na(lista)) %>%
  select(-year, -month)

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
  ) %>% 
  select(-tipo, -distrito)

##############################################################################
# ============== 3) Ayu_Seccion_2000.csv  ====================================
##############################################################################
df_2000 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2000/Ayu_Seccion_2000.csv") %>%
  rename_with(tolower) %>%
  rename(municipality = municipio, section = seccion, valids = validos) %>%
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
    PVEM_PCP       = cfcp,
    listanominal = "lista nominal",
    no_reg = "no regitrados"
  ) %>%
  mutate(turnout = total / listanominal) %>%
  select(-nulos, -no_reg)

#party vote validation

df_2000 <- df_2000 %>% 
  dplyr::mutate(PVEM = ifelse(!is.na(PVEM_PCP) & PVEM_PCP>0 & PVEM >0, 0, PVEM),
                PCP = ifelse(!is.na(PVEM_PCP) & PVEM_PCP>0 & PCP >0, 0, PCP))

# uniqueid
df_2000 <- df_2000 %>%
  mutate(uniqueid = case_when(
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
  ))


df_2000 <- df_2000 %>%
  mutate(
    year  = 2000,
    month = "July"
  ) %>% 
  select(-"tipo de casilla")


##############################################################################
# ============== 4) Ayu_Seccion_2003.csv  ====================================
##############################################################################
df_2003 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2003/Ayu_Seccion_2003.csv") %>%
  rename(municipality = MUNICIPIO, 
         section = SECCION, 
         no_reg = NoRegistrados,
         nulos = Nulos,
         total = Total,
         PRI_PVEM = "PRI-PVEM",
         listanominal = LISTANOMINAL) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0))

vars_2003 <- c("listanominal","nulos","no_reg","PAN","PRI","PRD","PT",
               "PVEM","PCP","PAS","PSN","PC", "cc1","cc2",
               "total")
for (v in vars_2003) {
  if (v %in% names(df_2003)) {
    df_2003[[v]] <- as.numeric(df_2003[[v]])
  }
}

df_2003 <- df_2003 %>%
  select(-SumaCC1, -SumaCC2)

# Step 1: Standardize coalition names to match party columns
df_2003 <- df_2003 %>%
  mutate(
    Coalicion1 = Coalicion1 %>%
      gsub("Convergencia", "PC", .) %>%
      gsub("APT-", "", .) %>% 
      gsub("-APT", "", .),
    Coalicion2 = Coalicion2 %>%
      gsub("Convergencia", "PC", .)
  )

# Step 2: Get unique coalitions (filter out empty strings and NAs)
coal1_unique <- unique(df_2003$Coalicion1[df_2003$Coalicion1 != "" & !is.na(df_2003$Coalicion1)])
coal2_unique <- unique(df_2003$Coalicion2[df_2003$Coalicion2 != "" & !is.na(df_2003$Coalicion2)])

# Initialize all possible coalition columns with 0
all_coalitions <- unique(c(coal1_unique, coal2_unique))
for (coal in all_coalitions) {
  if (is.na(coal) || coal == "") next
  col_name <- gsub("-", "_", coal)
  df_2003[[col_name]] <- 0
}

# Step 3: Process each row
for (i in 1:nrow(df_2003)) {
  # Handle Coalicion1
  if (!is.na(df_2003$Coalicion1[i]) && df_2003$Coalicion1[i] != "") {
    coal <- df_2003$Coalicion1[i]
    col_name <- gsub("-", "_", coal)
    parties <- strsplit(coal, "-")[[1]]
    
    # Add CC1 to coalition column
    df_2003[[col_name]][i] <- df_2003$CC1[i]
    
    # Set individual party columns to 0 for this row only
    for (party in parties) {
      if (party %in% names(df_2003)) {
        df_2003[[party]][i] <- 0
      }
    }
  }
  
  # Handle Coalicion2
  if (!is.na(df_2003$Coalicion2[i]) && df_2003$Coalicion2[i] != "") {
    coal <- df_2003$Coalicion2[i]
    col_name <- gsub("-", "_", coal)
    parties <- strsplit(coal, "-")[[1]]
    
    # Add CC2 to coalition column
    df_2003[[col_name]][i] <- df_2003[[col_name]][i] + df_2003$CC2[i]
    
    # Set individual party columns to 0 for this row only
    for (party in parties) {
      if (party %in% names(df_2003)) {
        df_2003[[party]][i] <- 0
      }
    }
  }
}

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
  ) %>% 
  select(-Coalicion1, -Coalicion2, -CC1, -CC2)

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2003 <- df_2003 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(listanominal:PRD_PT_PSN_PC), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2003 <- collapsed_2003 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(c(PAN:PRI_PVEM, PRD_PT_PC:PRD_PT_PSN_PC)), na.rm = TRUE),
    year = 2003,
    month = "July"
  )

##############################################################################
# ============== 6) Ayu_Seccion_2006.csv  ====================================
##############################################################################
df_2006 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2006/Ayu_Seccion_2006.csv") %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal,
    total = Total,
    nulos = Nulos,
    noreg = NoRegistrados
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0)) %>% 
  rename_with(~ gsub("-", "_", .x)) %>% 
  rename_with(~ gsub("PNA", "PANAL", .x)) %>%
  rename_with(~ gsub("Convergencia", "PC", .x)) %>% 
  select(-c(id_entidad_electoral:id_distrito), -id_casilla, -tipo_casilla, -"descripcion...10", -ext_contigua, -c(estatus_casilla:"descripcion...19"),-id_municipio)

vars_2006 <- c(
  "listanominal",  "nulos",  "PAN",  "PRI",  "PRD_PT_PC",  "PVEM",  "PCP",
  "PANAL",  "PASC",  "PAN_PANAL",  "PAN_PCP_PANAL",  "PCP_PANAL",  "PRI_PASC",  "PRI_PCP_PASC",
  "total",  "noreg"
)

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

# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2006 & edon == 24) %>% 
  select(yr, inegi, coal1, coal2, coal3, coal4) %>% 
  rename(
    year = yr,
    uniqueid = inegi) %>% 
  mutate(
    across(
      coal1:coal4,
      ~ str_replace_all(., "-", "_") |> 
        str_replace_all(regex("PNA", ignore_case = TRUE), "PANAL") |> 
        str_replace_all(regex("CONVE", ignore_case = TRUE), "PC") |> 
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
df_2006 <- process_coalitions(df_2006, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


##############################################################################
# ============== 7) Ayu_Seccion_2009.csv  ====================================
##############################################################################
df_2009 <- read_csv("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2009/Ayu_Seccion_2009.csv") %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal,
    total = Total,
    noreg = NoRegistrados,
    nulos = Nulos
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%
  filter(!(is.na(total) | total==0)) %>% 
  rename_with(~ gsub(" - ", "_", .x)) %>% 
  rename_with(~ gsub("-", "_", .x)) %>% 
  rename_with(~ gsub("PNA", "PANAL", .x)) %>% 
  rename_with(~ gsub("Convergencia", "PC", .x)) %>% 
  select(-c(id_entidad_electoral:id_distrito, id_casilla:id_municipio, estatus_casilla:"descripcion...19" ))

vars_2009 <- c(
  "listanominal", "nulos", "noreg", "PAN", "PRI", "PRD", "PT", "PVEM",
  "PCP", "PC", "PANAL", "PSD", "PAN_PANAL", "PAN_PCP_PANAL", "PAN_PT_PANAL",
  "PANAL_PSD", "PRD_PC", "PRD_PCP", "PRD_PCP_PC", "PRD_PT", "PRD_PT_PCP",
  "PRD_PT_PCP_PSD", "PRI_PANAL","PRI_PSD", "PRI_PVEM", "PT_PC", "PT_PCP",
  "PT_PCP_PC", "PT_PSD", "PVEM_PCP", "PVEM_PSD", "total"
)

for (v in vars_2009) {
  if (v %in% names(df_2009)) {
    df_2009[[v]] <- as.numeric(df_2009[[v]])
  }
}


df_2009 <- df_2009 %>%
  mutate(turnout = total/listanominal)

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
    ),
    year  = 2009,
    month = "July"
  )

# Apply coalition processing function
df_2009 <- process_coalitions(df_2009, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

##############################################################################
# ============== 8) Ayu_Seccion_2012.dta  ====================================
##############################################################################
df_2012 <- read_dta("../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Other/Ayu_Seccion_2012.dta") %>%
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


all_slp <- bind_rows(df_1997, df_2000, collapsed_2003)

# append the rest
all_slp <- bind_rows(all_slp, df_2006, df_2009, df_2012)

################################################################################
# 2015 PROCESSING - SALVADOR lines 1-275
# Excel file with multiple sheets
################################################################################

# Read all sheets from 2015 Excel file
xlsx_path_2015 <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2015/Ayuntamientos_2015.xlsx"
sheets_2015 <- excel_sheets(xlsx_path_2015)

df_2015_list <- lapply(sheets_2015, function(sheet) {
  tryCatch({
    temp <- read_excel(xlsx_path_2015, sheet = sheet, col_types = "text") %>%
      filter(!is.na(Seccion), Seccion != "") %>%
      mutate(across(everything(), ~replace(., . == "", "0")))
    return(temp)
  }, error = function(e) return(NULL))
})

df_2015 <- bind_rows(df_2015_list) %>%
  filter(!is.na(Municipio), Municipio != "") %>%
  select(-any_of(c("EXCEDE NO DE BOLETAS", "Porcentaje de Votacion Emitida", 
                   "Porcentaje de Votos Nulos", "VOTACION NULA MAYOR AL 5%", 
                   "CAPTURADA", "VOTACION VALIDA EMITIDA"))) %>%
  rename(municipality = Municipio, section = Seccion,
         nulos = "VOTOS NULOS",
         total = "VOTACION EMITIDA",
         CI_1 = CI,
         listanominal = "Lista Nominal") %>%
  mutate(across(-municipality, as.numeric)) %>%
  mutate(municipality = toupper(remove_accents(municipality))) %>% 
  rename_with(~ gsub("PNA", "PANAL", .x)) %>% 
  rename_with(~ gsub("PMC", "MC", .x))

# Rename PNA to PANAL per SALVADOR
if ("PNA" %in% names(df_2015)) {
  df_2015 <- df_2015 %>% rename(PANAL = PNA)
}

if ("PMC" %in% names(df_2015)) {
  df_2015 <- df_2015 %>% rename(MC = PMC)
}

# Assign uniqueid
df_2015 <- df_2015 %>%
  mutate(uniqueid = assign_slp_uniqueid(municipality))

# Collapse by municipality, uniqueid, section
df_2015 <- df_2015 %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Standardize column names
party_cols_2015 <- c("PAN", "PRI", "PRD", "PT", "PVEM", "PCP", "PANAL", "PH", "MORENA", "PES", "MC")
df_2015 <- df_2015 %>%
  rename_with(toupper) %>%
  rename(municipality = MUNICIPALITY, uniqueid = UNIQUEID, section = SECTION,
         listanominal = LISTANOMINAL, total = TOTAL, nulos = NULOS)

# Calculate valid votes
df_2015 <- df_2015 %>%
  mutate(
    valid = rowSums(select(., any_of(party_cols_2015)), na.rm = TRUE),
    turnout = total/listanominal,
    year = 2015, month = "June", STATE = "SAN LUIS POTOSI"
  ) %>% 
  select(-c("DTO LOCAL":"TIPO"), -"FORMULAS NO REGISTRADAS", -"STATE")

# Apply coalition processing function
df_2015 <- process_coalitions(df_2015, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

cat("2015:", nrow(df_2015), "rows\n")

################################################################################
# 2018 PROCESSING - SALVADOR lines 282-605
# Excel file with multiple sheets
################################################################################

# Read all sheets from 2018 Excel file
xlsx_path_2018 <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2018/MunicipiosSLP_2018.xlsx"
sheets_2018 <- excel_sheets(xlsx_path_2018)

df_2018_list <- lapply(sheets_2018, function(sheet) {
  tryCatch({
    temp <- read_excel(xlsx_path_2018, sheet = sheet, col_types = "text") %>%
      mutate(municipality = sheet) %>%
      filter(!is.na("CASILLA / ACTA"), "CASILLA / ACTA" != "") %>%
      mutate(across(everything(), ~replace(., . == "", "0")))
    return(temp)
  }, error = function(e) return(NULL))
})

df_2018 <- bind_rows(df_2018_list) %>%
  rename(CASILLAACTA = "CASILLA / ACTA") %>% 
  select(-any_of(c("O", "P", "section", "CASILLA /ACTA"))) %>%
  mutate(SECCION = substr(CASILLAACTA, 1, 4)) %>%
  mutate(across(-c(municipality, CASILLAACTA, SECCION), as.numeric)) %>% 
  mutate(
    `PRI-PVEM-PCP` = rowSums(across(any_of(c("PRI-PVEM-PCP", "PRI,PVEM,PCP"))), na.rm = TRUE),
    `PVEM-PNA`     = rowSums(across(any_of(c("PVEM-PNA",     "PVEM,PNA"))),     na.rm = TRUE),
    `PRI-PVEM`     = rowSums(across(any_of(c("PRI-PVEM",     "PRI,PVEM"))),     na.rm = TRUE),
    `PRI-PNA`      = rowSums(across(any_of(c("PRI-PNA",      "PRI,PNA"))),      na.rm = TRUE)
  ) %>%
  select(-any_of(c("PRI,PVEM,PCP", "PVEM,PNA", "PRI,PVEM", "PRI,PNA"))) %>%
  rename_with(~ gsub("[-,]", "", .))

# Fix row 3011 per SALVADOR
if (nrow(df_2018) >= 3011 && "PTMORENAPES" %in% names(df_2018)) {
  df_2018$PTMORENAPES[3011] <- 0
}

# Rename columns per SALVADOR
df_2018 <- df_2018 %>%
  rename_with(~gsub("PMC", "MC", .)) %>%
  rename_with(~gsub("PNA", "PANAL", .))

# Merge with uniqueids
uniqueids_slp <- read_excel(
  "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/Other/uniqueids.xlsx"
)

df_2018 <- df_2018 %>%
  left_join(uniqueids_slp, by = "municipality") %>%
  mutate(municipality = coalesce(municipality2, municipality)) %>%
  select(-municipality2)

# Process coalitions per SALVADOR
# PAN-PRD-MC coalition
if (all(c("PANPRDMC", "PANPRD", "PRDMC", "PANMC", "PAN", "PRD", "MC") %in% names(df_2018))) {
  df_2018 <- df_2018 %>%
    mutate(
      PANPRDMC = if_else(!is.na(PANPRDMC), 
                         PANPRDMC + coalesce(PANPRD, 0) + coalesce(PRDMC, 0) + 
                           coalesce(PANMC, 0) + coalesce(PAN, 0) + coalesce(PRD, 0) + coalesce(MC, 0),
                         PANPRDMC),
      PANPRD = if_else(!is.na(PANPRDMC), NA_real_, PANPRD),
      PRDMC = if_else(!is.na(PANPRDMC), NA_real_, PRDMC),
      PANMC = if_else(!is.na(PANPRDMC), NA_real_, PANMC),
      MC = if_else(!is.na(PANPRDMC), NA_real_, MC),
      PAN = if_else(!is.na(PANPRDMC), NA_real_, PAN),
      PRD = if_else(!is.na(PANPRDMC), NA_real_, PRD)
    ) %>%
    select(-PANPRD, -PRDMC) %>%
    rename(PAN_PRD_MC = PANPRDMC, PAN_MC = PANMC)
}

# PT-MORENA-PES coalition
if (all(c("PTMORENAPES", "PTMORENA", "PTPES", "MORENAPES", "PT", "MORENA", "PES") %in% names(df_2018))) {
  df_2018 <- df_2018 %>%
    mutate(
      PTMORENAPES = if_else(!is.na(PTMORENAPES),
                            PTMORENAPES + coalesce(PTMORENA, 0) + coalesce(PTPES, 0) +
                              coalesce(MORENAPES, 0) + coalesce(PT, 0) + coalesce(MORENA, 0) + coalesce(PES, 0),
                            PTMORENAPES),
      PT = if_else(!is.na(PTMORENAPES), NA_real_, PT),
      MORENA = if_else(!is.na(PTMORENAPES), NA_real_, MORENA),
      PES = if_else(!is.na(PTMORENAPES), NA_real_, PES)
    ) %>%
    select(-any_of(c("PTMORENA", "PTPES", "MORENAPES", "PES"))) %>%
    rename(PT_MORENA_PES = PTMORENAPES)
}

# PRI coalition processing
if (all(c("PRIPVEMPCP", "PRIPVEMPANAL", "PVEMPANAL", "PRIPVEM", "PRIPANAL", 
          "PRIPANALPCP", "PRIPVEMPANALPCP", "PRIPCP") %in% names(df_2018))) {
  df_2018 <- df_2018 %>%
    rename(
      PRI_PVEM_PCP = PRIPVEMPCP,
      PRI_PVEM_PANAL = PRIPVEMPANAL,
      PVEM_PANAL = PVEMPANAL,
      PRI_PVEM = PRIPVEM,
      PRI_PANAL = PRIPANAL,
      PRI_PANAL_PCP = PRIPANALPCP,
      PRI_PVEM_PANAL_PCP = PRIPVEMPANALPCP,
      PRI_PCP = PRIPCP
    )
}

# Rename CI column
if ("CI" %in% names(df_2018)) {
  df_2018 <- df_2018 %>% rename(CI_1 = CI)
}

non_vote_cols <- c("obs", "CASILLAACTA", "SECCION", "municipality", "uniqueid")


# Calculate total per SALVADOR
df_2018 <- df_2018 %>%
  mutate(
    uniqueid = if_else(is.na(uniqueid), assign_slp_uniqueid(municipality), uniqueid),
    total = rowSums(across(-any_of(non_vote_cols)), na.rm = TRUE)
  )

# Collapse
df_2018 <- df_2018 %>%
  rename(section = SECCION) %>%
  mutate(section = as.numeric(section)) %>%
  select(-any_of(c("CNR", "VTN", "CASILLAACTA", "obs"))) %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
  filter(section > 0)

# Merge with lista nominal
ln_2018 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE == "SAN LUIS POTOSI") %>%
  rename(listanominal = ListadoNominalINE) %>%
  select(section, listanominal)

df_2018 <- df_2018 %>%
  left_join(ln_2018, by = "section")

# Calculate valid and finalize
party_cols_2018 <- c("PAN_MC", "PRI", "PRD", "PT", "PVEM", "PCP", "PANAL", "MORENA", 
                     "PT_MORENA_PES", "PAN", "PRI_PVEM_PCP", "MC", "PAN_PRD_MC",
                     "PRI_PVEM_PANAL", "PVEM_PANAL", "PRI_PVEM", "PRI_PANAL",
                     "PRI_PANAL_PCP", "PRI_PVEM_PANAL_PCP", "PRI_PCP", "CI_1")

df_2018 <- df_2018 %>%
  mutate(
    valid = rowSums(select(., any_of(party_cols_2018)), na.rm = TRUE),
    turnout = if(!is.na(listanominal[1])) total / listanominal else NA_real_,
    year = 2018, month = "July", STATE = "SAN LUIS POTOSI"
  )

cat("2018:", nrow(df_2018), "rows\n")


#####################################
### PROCESSING DATA FOR 2021 -------
#####################################

library(purrr)
library(stringr)

# Set the directory path
data_dir <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2021/"

# Get all Excel files in the directory
excel_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Function to read and process each file
read_municipality_file <- function(file_path) {
  # Extract municipality name from filename
  municipality_name <- basename(file_path) %>%
    str_remove("\\.xlsx$") %>%
    str_remove("^CME ") # Remove "CME " prefix if present
  
  # Read the Excel file with all columns as text to avoid type conflicts
  df <- read_excel(file_path, col_types = "text")
  
  # Add municipality column
  df <- df %>%
    mutate(source = municipality_name)
  
  return(df)
}

# Read all files and combine them
data_2021 <- map_dfr(excel_files, read_municipality_file) %>%
  mutate(
    # Consolidate municipality columns
    municipality = case_when(
      !is.na(Municipio) ~ Municipio,
      !is.na(Municipo) ~ Municipo,
      TRUE ~ NA_character_
    ),
    
    # Consolidate total columns
    total = case_when(
      !is.na(Total) ~ Total,
      !is.na(TOT) ~ TOT,
      TRUE ~ NA_character_
    )
  ) %>%
  # Remove original columns
  select(-c(Municipio, Municipo, Total, TOT, "Dto. Local", "Dto. local","Dto.Local", "Dto. Loc", "No. Mpo.", source))

vote_cols <- c("PAN","PRI","PRD","PT","PVEM","PCP","PMC","PNA","MORENA","PES",
               "RSP","FXM","CNR","VTN","CI","PT,PVEM",
               "PAN,PRI,PRD,PCP","PAN,PRI,PRD","PAN,PRI,PCP","PAN,PRD,PCP",
               "PRI,PRD,PCP","PAN,PRI","PAN,PRD","PAN,PCP","PRI,PRD",
               "PRI,PCP","PRD,PCP",
               "ALIANZA PAN,PRD","ALIANZA PRI,PCP","ALIANZA PAN,PCP",
               "ALIANZA PRI,PRD,PCP","ALIANZA PAN,PRI","ALIANZA PAN,PRI,PCP")
# Rename columns
data_2021 <- data_2021 %>%
  mutate(
    across(all_of(vote_cols), as.numeric),
    `PAN,PRI,PCP` = rowSums(across(any_of(c("PAN,PRI,PCP", "ALIANZA PAN,PRI,PCP"))), na.rm = TRUE),
    `PRI,PRD,PCP` = rowSums(across(any_of(c("PRI,PRD,PCP", "ALIANZA PRI,PRD,PCP"))), na.rm = TRUE),
    `PAN,PRI` = rowSums(across(any_of(c("PAN,PRI", "ALIANZA PAN,PRI"))), na.rm = TRUE),
    `PAN,PRD` = rowSums(across(any_of(c("PAN,PRD", "ALIANZA PAN,PRD"))), na.rm = TRUE),
    `PAN,PCP` = rowSums(across(any_of(c("PAN,PCP", "ALIANZA PAN,PCP"))), na.rm = TRUE),
    `PAN,PRI,PRD,PCP` = rowSums(across(any_of(c("PAN,PRI,PRD,PCP", "ALIANZA PAN,PRI,PRD,PCP"))), na.rm = TRUE),
    `PRI,PCP` = rowSums(across(any_of(c("PRI,PCP", "ALIANZA PRI,PCP"))), na.rm = TRUE)
  ) %>%
  select(-any_of(c("ALIANZA PAN,PRI,PCP", "ALIANZA PRI,PRD,PCP", 
                   "ALIANZA PAN,PRI", "ALIANZA PAN,PRD", "ALIANZA PAN,PCP",
                   "ALIANZA PAN,PRI,PRD,PCP", "ALIANZA PRI,PCP"))) %>%
  dplyr::rename(no_reg = CNR,
                nulos = VTN,
                CI_1 = CI,
                PANAL = PNA) %>%
  rename_with(~ gsub("ALIANZA ", "", .x)) %>%
  rename_with(~ gsub(",", "_", .x)) %>% 
  dplyr::mutate(
    section = as.numeric(str_extract(Casilla, "\\d{4}(?=[A-Z])")),
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    municipality = case_when(municipality == "AHULULCO" ~ "AHUALULCO",
                             TRUE ~ municipality),
    total = as.numeric(total)
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2021 <- data_2021 %>% 
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
      str_detect(municipality, "MEXQUITIC") ~ 24021,
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
      str_detect(municipality, "TAMPAMOLON") ~ 24039,
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
      TRUE ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2021 <- data_2021 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:total, total), 
           \(x) sum(x, na.rm = TRUE))
  )

# Load the Lista Nominal 2021 data and filter by criteria
ln_2021 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef21.xlsx", skip = 3, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "col_l",
                                    "listanominal", "col_n", "col_o", "col_p")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 24) %>%
  dplyr::select(section,listanominal)

# Merge Lista Nominal data with the collapsed data
collapsed_2021 <- collapsed_2021 %>%
  left_join(ln_2021, by = "section")

# Calculate valid votes and final details
collapsed_2021 <- collapsed_2021 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(c(PAN:PRD_PCP, MORENA:CI_1)), na.rm = TRUE),
    year = 2021,
    month = "June"
  )

# Apply coalition processing function
collapsed_2021 <- process_coalitions(collapsed_2021, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

#####################################
### PROCESSING DATA FOR 2024 -------
#####################################

# Set the directory path for 2024 files
data_dir_24 <- "../../../Data/Raw Electoral Data/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018,2021,2024/2024/"

# Get all Excel files in the directory
excel_files_24 <- list.files(data_dir_24, pattern = "\\.xlsx$", full.names = TRUE)


# Function to read and process each 2024 file
read_municipality_file_24 <- function(file_path) {
  # Extract municipality name from filename (remove number prefix and .xlsx)
  municipality_name <- basename(file_path) %>%
    str_remove("\\.xlsx$") %>%
    str_remove("^\\d+-") # Remove number and dash prefix like "1-"
  
  # Read the Excel file with all columns as text
  df <- read_excel(file_path, col_types = "text")
  
  # Add municipality column
  df <- df %>%
    mutate(source = municipality_name)
  
  return(df)
}

# Read all files and combine them
data_2024_cons <- map_dfr(excel_files_24, read_municipality_file_24)

# Rename columns
data_2024 <- data_2024_cons %>%
  dplyr::rename(municipality = Municipio,
                no_reg = CNR,
                nulos = VTN,
                CI_1 = CI,
                PANAL = PNASLP,
                PCP = CP,
                MC = PMC,
                PES = PESSLP) %>%
  rename_with(~ gsub(",", "_", .x)) %>% 
  dplyr::mutate(
    section = as.numeric(str_sub(casilla, 2, 5)),
    municipality = toupper(municipality),
    municipality = gsub("Á", "A", municipality),
    municipality = gsub("É", "E", municipality),
    municipality = gsub("Í", "I", municipality),
    municipality = gsub("Ó", "O", municipality),
    municipality = gsub("Ú", "U", municipality),
    municipality = gsub("Ü", "U", municipality),
    municipality = gsub("Ñ", "N", municipality),
    across(PAN:nulos, as.numeric),
    across(PAN_PRI_PRD:CI_1, as.numeric),
  ) %>% 
  dplyr::filter(section > 0)

# Assign uniqueids
data_2024 <- data_2024 %>% 
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
      str_detect(municipality, "MEXQUITIC") ~ 24021,
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
      str_detect(municipality, "TAMPAMOLON") ~ 24039,
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
      TRUE ~ NA
    )
  )

# Group by municipality, section, and uniqueid, and sum the relevant columns
collapsed_2024 <- data_2024 %>%
  dplyr::group_by(municipality, section, uniqueid) %>%
  dplyr::summarise(
    across(c(PAN:nulos, PAN_PRI_PRD:CI_1), 
           \(x) sum(x, na.rm = TRUE))
  )

# Load the Lista Nominal 2024 data and filter by criteria
ln_2024 <- read_excel("../../../Data/Raw Electoral Data/Listas Nominales/listanom_pef24.xlsx", skip = 2, 
                      col_names = c("state_code", "district_code", "mun_code", 
                                    "section", "col_e", "col_f", "col_g", "col_h", 
                                    "col_i", "col_j", "col_k", "listanominal")) %>%
  dplyr::select(state_code, mun_code, section, listanominal) %>% 
  dplyr::filter(state_code == 24) %>%
  dplyr::select(section,listanominal)

# Merge Lista Nominal data with the collapsed data
collapsed_2024 <- collapsed_2024 %>%
  left_join(ln_2024, by = "section")

# Calculate valid votes and final details
collapsed_2024 <- collapsed_2024 %>%
  dplyr::mutate(
    total = sum(c_across(c(PAN:CI_1)), na.rm = TRUE),
    turnout = total/listanominal,
    valid = sum(c_across(c(PAN:PT_MORENA, PAN_PRI_PRD:CI_1)), na.rm = TRUE),
    year = 2024,
    month = "June"
  )

# Apply coalition processing function
collapsed_2024 <- process_coalitions(collapsed_2024, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)


##############################################################################
# ============== 12) Append 2 ===============
##############################################################################

all_slp_final <- bind_rows(all_slp,df_2015, df_2018, collapsed_2021, collapsed_2024)

##############################################################################
# Done
##############################################################################


data.table::fwrite(all_slp_final,"../../../Processed Data/sanluispotosi/slp_process_raw_data.csv")