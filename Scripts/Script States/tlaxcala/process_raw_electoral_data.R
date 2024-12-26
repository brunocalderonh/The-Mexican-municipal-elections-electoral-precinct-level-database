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

################################################################################
# 1) Read Excel "Ayu_Seccion_2001_No_LN.xlsx" (Equivalent to "import excel"),
#    rename columns, drop certain rows, parse numeric columns, sum, etc.
################################################################################

# We'll assume the file is named "Ayu_Seccion_2001_No_LN.xlsx", first row as col names.
df_tlx <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2001_No_LN.xlsx",
  sheet = 1,             # or specify a sheet name if not the first
  col_names = TRUE
) %>%
  as.data.frame()
colnames(df_tlx) <- tolower(colnames(df_tlx))
names(df_tlx) <- gsub("[- ]", "", names(df_tlx))

# rename municipio->municipality, seccion->section
df_tlx <- df_tlx %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

# drop if municipality=="" & section==.
df_tlx <- df_tlx %>%
  filter(!(municipality=="" & is.na(section)))

# Convert columns (e.g., pri through nulos) to numeric
# We'll identify them by name or by the range of columns that correspond.
# In Stata, code is: destring pri - nulos, replace
# We'll do partial approach if we know the columns:
vote_cols <- c("pri","pan","prd","pt","pvem","partidodemocrata","psn","pc","pas","pcdt","pjs","nulos")
df_tlx <- df_tlx %>%
  mutate(across(all_of(vote_cols), as.numeric))

# create total= rowtotal(...) => rowwise sum
df_tlx <- df_tlx %>%
  rowwise() %>%
  mutate(
    total = sum(
      c_across(c("pan","pri","prd","pt","pvem","partidodemocrata","psn","pc","pas","pcdt","pjs","nulos")),
      na.rm=TRUE
    )
  ) %>%
  ungroup()

# drop if total==. or total==0
df_tlx <- df_tlx %>%
  filter(!(is.na(total) | total==0))

# collapse (sum) pri - nulos total, by (municipality, section)
# We'll do group_by + summarise in R.
df_collapsed <- df_tlx %>%
  group_by(municipality, section) %>%
  summarise(
    across(c(pri, pan, prd, pt, pvem, partidodemocrata, psn, pc, pas, pcdt, pjs, nulos, total),
           sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 2) Rename columns to match Stata code
################################################################################
df_collapsed <- df_collapsed %>%
  rename(
    PAN               = pan,
    PRI               = pri,
    PRD               = prd,
    PT                = pt,
    PVEM              = pvem,
    PC                = pc,
    Partido_Democrata = partidodemocrata,  # rename partidodemocrata -> Partido_Democrata
    PCDT              = pcdt,
    PSN               = psn,
    PAS               = pas,
    PJS               = pjs
    # nulos remains nulos, total remains total
  )

# drop nulos
df_collapsed <- df_collapsed %>%
  select(-nulos)

################################################################################
# 3) Create uniqueid=0, then assign municipality codes
################################################################################
df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0)

# Then we do a large case_when for each municipality
df_collapsed <- df_collapsed %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"      ~ 29022,
      municipality=="AMAXAC DE GUERRERO"               ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"  ~ 29002,
      municipality=="APIZACO"                          ~ 29005,
      municipality=="ATLANGATEPEC"                     ~ 29003,
      municipality=="ALTZAYANCA"                       ~ 29004,
      municipality=="BENITO JUÁREZ"                    ~ 29045,
      municipality=="CALPULALPAN"                      ~ 29006,
      municipality=="CHIAUTEMPAN"                      ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"          ~ 29018,
      municipality=="CUAPIAXTLA"                       ~ 29008,
      municipality=="CUAXOMULCO"                       ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"           ~ 29007,
      municipality=="EMILIANO ZAPATA"                  ~ 29046,
      municipality=="ESPAÑITA"                         ~ 29012,
      municipality=="HUAMANTLA"                        ~ 29013,
      municipality=="HUEYOTLIPAN"                      ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS" ~ 29015,
      municipality=="IXTENCO"                          ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"         ~ 29048,
      municipality=="LÁZARO CÁRDENAS"                  ~ 29047,
      municipality=="MAZATECOCHCO DE JOSÉ MARÍA MORELOS" ~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"          ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"    ~ 29021,
      municipality=="NATIVITAS"                        ~ 29023,
      municipality=="PANOTLA"                          ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"        ~ 29041,
      municipality=="SAN DAMIÁN TEXOLOC"               ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"        ~ 29050,
      municipality=="SAN JERÓNIMO ZACUALPAN"           ~ 29051,
      municipality=="SAN JOSÉ TEACALCO"                ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"              ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"         ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"              ~ 29055,
      municipality=="SAN PABLO DEL MONTE"              ~ 29025,
      municipality=="SANCTORUM"                        ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"              ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"          ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"          ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"             ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"              ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"          ~ 29060,
      municipality=="TENANCINGO"                       ~ 29027,
      municipality=="SAN LUIS TEOLOCHOLCO"             ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"          ~ 29019,
      municipality=="TEPEYANCO"                        ~ 29029,
      municipality=="TERRENATE"                        ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"          ~ 29031,
      municipality=="TETLATLAHUCA"                     ~ 29032,
      municipality=="TLAXCALA"                         ~ 29033,
      municipality=="TLAXCO"                           ~ 29034,
      municipality=="TOCATLÁN"                         ~ 29035,
      municipality=="TOTOLAC"                          ~ 29036,
      municipality=="TZOMPANTEPEC"                     ~ 29038,
      municipality=="XALOZTOC"                         ~ 29039,
      municipality=="XALTOCAN"                         ~ 29040,
      municipality=="XICOHTZINCO"                      ~ 29042,
      municipality=="YAUHQUEMEHCAN"                    ~ 29043,
      municipality=="ZACATELCO"                        ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SÁNCHEZ SANTOS" ~ 29037,
      TRUE                                            ~ uniqueid
    )
  )

################################################################################
# 4) Compute valid= rowtotal(PRI PAN PRD PT PVEM Partido_Democrata PSN PC PAS PCDT PJS)
################################################################################
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PRI","PAN","PRD","PT","PVEM","Partido_Democrata",
                           "PSN","PC","PAS","PCDT","PJS")), na.rm=TRUE)
  ) %>%
  ungroup()

# "drop if section==150" => remove row(s) with section=150
df_collapsed <- df_collapsed %>%
  filter(section != 150)

################################################################################
# 5) Merge with "..\..\all_months_years.dta" for (ed=29, month=9, year=2001),
#    rename columns, keep if month==9 & year==2001, drop if unmatched
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(
    ed      = 29,
    seccion = section
  )

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

# We'll do a left_join => in Stata it's "capture merge 1:m ed seccion"
df_merged <- df_collapsed %>%
  left_join(df_all, by=c("ed","seccion"))

# keep if month==9 & year==2001
df_merged <- df_merged %>%
  filter(month==9, year==2001)

# drop if _merge==2 => in R, means dropping rows with NA in 'lista'
df_merged <- df_merged %>%
  filter(!is.na(lista))

df_merged <- df_merged %>%
  select(-ed, -seccion, -year, -month)

# rename lista->listanominal
df_2001<- df_merged %>%
  rename(listanominal=lista) %>%
  mutate(
    year  = 2001,
    month = "November"
  ) %>%
  arrange(section)

################################################################################
# Part A: Read "Ayu_Seccion_2004_LN.csv", rename seccion->section, sort, save .dta
################################################################################

# In Stata:
# insheet using Ayu_Seccion_2004_LN.csv, clear
# rename seccion section
# sort section
# save Ayu_Seccion_2004_LN.dta, replace

df_ln <- read_csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2004_LN.csv", show_col_types=FALSE) 

colnames(df_ln) <- tolower(colnames(df_ln))
names(df_ln) <- gsub("[- ]", "", names(df_ln))

df_ln <-df_ln %>%
  rename(section = seccion) %>%
  arrange(section)

################################################################################
# Part B:
################################################################################

df_no_ln <- read_csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2004_No_LN.csv", show_col_types=FALSE)
colnames(df_no_ln) <- tolower(colnames(df_no_ln))
names(df_no_ln) <- gsub("[- ]", "", names(df_no_ln))

df_no_ln <-df_no_ln %>%
  rename(
    municipality = municipio,
    section      = seccion,
    total        = emitidos
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%  # drop if municipality=="" & section==.
  filter(!(is.na(total) | total==0))               # drop if total==. | total==0

# parse (pan - validos) => columns from 'pan' to 'validos'
# We'll find those columns if needed or list them explicitly:
vote_cols <- c("pan","pri","pripvem","prd","pt","ptpcdtpjs","ptpcdt","ptpjs",
               "pvem","pc","pcdt","pcdtpjs","pjs","anulados","validos")

df_no_ln <- df_no_ln %>%
  mutate(across(all_of(intersect(vote_cols,names(.))), as.numeric))

# collapse sum by (municipality, section)
df_no_ln_collapse <- df_no_ln %>%
  group_by(municipality, section) %>%
  summarise(across(intersect(vote_cols,c("total")), sum, na.rm=TRUE), .groups="drop")

# merge on 'section'
df_merged1 <- df_no_ln_collapse %>%
  arrange(section) %>%
  left_join(df_ln, by="section")


################################################################################
# Part C: Merge with all_months_years (ed=29, month=10, year=2004), keep if matched
################################################################################


df_merged2 <- df_merged1 %>%
  mutate(
    ed      = 29,
    seccion = section
  )

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista) %>%
  filter(month==10, year==2004) 

df_joined <- df_merged2 %>%
  left_join(df_all, by=c("ed","seccion"))

df_joined <- df_joined %>%
  select(-ed, -seccion, -year, -month, -starts_with("_merge"))

# "replace listanominal = lista if listanominal==."
# in R, we do:
# if there's a column "listanominal" in df_joined, we do:
if ("listanominal" %in% names(df_joined)) {
  df_joined <- df_joined %>%
    mutate(
      listanominal = if_else(is.na(listanominal), lista, listanominal)
    ) %>%
    select(-lista)
} else {
  # rename lista->listanominal if there's no 'listanominal' yet
  df_joined <- df_joined %>%
    rename(listanominal = lista)
}


################################################################################
# Part D: Rename columns, compute turnout=total/listanominal, drop columns, assign uniqueid
################################################################################

# rename columns as per Stata
df_final <- df_joined %>%
  rename(
    PAN               = pan,
    PRI               = pri,
    PRI_PVEM          = pripvem,
    PRD               = prd,
    PT                = pt,
    PT_PCDT_PJS       = ptpcdtpjs,
    PT_PCDT           = ptpcdt,
    PT_PJS            = ptpjs,
    PVEM              = pvem,
    PC                = pc,
    PCDT              = pcdt,
    PCDT_PJS          = pcdtpjs,
    PJS               = pjs
  )

# drop 'anulados', 'validos' if exist
df_final <- df_final %>%
  select(-any_of(c("anulados","validos")))

# gen turnout= total/listanominal
df_final <- df_final %>%
  mutate(turnout = total / listanominal)

# gen uniqueid=0 => then replace codes
df_final <- df_final %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"       ~ 29022,
      municipality=="AMAXAC DE GUERRERO"                ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"   ~ 29002,
      municipality=="APIZACO"                           ~ 29005,
      municipality=="ATLANGATEPEC"                      ~ 29003,
      municipality=="ATLTZAYANCA"                       ~ 29004,  # note the difference from "ALTZAYANCA" above
      municipality=="BENITO JUAREZ"                     ~ 29045,
      municipality=="CALPULALPAN"                       ~ 29006,
      municipality=="CHIAUTEMPAN"                       ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"           ~ 29018,
      municipality=="CUAPIAXTLA"                        ~ 29008,
      municipality=="CUAXOMULCO"                        ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"            ~ 29007,
      municipality=="EMILIANO ZAPATA"                   ~ 29046,
      municipality=="ESPANITA"                          ~ 29012,
      municipality=="HUAMANTLA"                         ~ 29013,
      municipality=="HUEYOTLIPAN"                       ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS"  ~ 29015,
      municipality=="IXTENCO"                           ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"          ~ 29048,
      municipality=="LAZARO CARDENAS"                  ~ 29047,
      municipality=="MAZATECOCHCO DE JOSE MARIA MORELOS"~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"           ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"     ~ 29021,
      municipality=="NATIVITAS"                         ~ 29023,
      municipality=="PANOTLA"                           ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"         ~ 29041,
      municipality=="SAN DAMIAN TEXOLOC"                ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"         ~ 29050,
      municipality=="SAN JERONIMO ZACUALPAN"            ~ 29051,
      municipality=="SAN JOSE TEACALCO"                 ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"               ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"          ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"               ~ 29055,
      municipality=="SAN PABLO DEL MONTE"               ~ 29025,
      municipality=="SANCTORUM"                         ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"               ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"           ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"           ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"              ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"               ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"           ~ 29060,
      municipality=="TENANCINGO"                        ~ 29027,
      municipality=="SAN LUIS TEOLOCHOLCO"              ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"           ~ 29019,
      municipality=="TEPEYANCO"                         ~ 29029,
      municipality=="TERRENATE"                         ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"           ~ 29031,
      municipality=="TETLATLAHUCA"                      ~ 29032,
      municipality=="TLAXCALA"                          ~ 29033,
      municipality=="TLAXCO"                            ~ 29034,
      municipality=="TOCATLAN"                          ~ 29035,
      municipality=="TOTOLAC"                           ~ 29036,
      municipality=="TZOMPANTEPEC"                      ~ 29038,
      municipality=="XALOZTOC"                          ~ 29039,
      municipality=="XALTOCAN"                          ~ 29040,
      municipality=="XICOHTZINCO"                       ~ 29042,
      municipality=="YAUHQUEMEHCAN"                     ~ 29043,
      municipality=="ZACATELCO"                         ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS" ~ 29037,
      TRUE                                             ~ uniqueid
    )
  )

# compute valid= rowtotal(PAN PRI PRD PT PVEM PC PCDT PJS PRI_PVEM PT_PCDT_PJS PCDT_PJS PT_PCDT PT_PJS)
# gen year=2004, month="November"
df_2004 <- df_final %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(
      "PAN","PRI","PRD","PT","PVEM","PC","PCDT","PJS","PRI_PVEM",
      "PT_PCDT_PJS","PCDT_PJS","PT_PCDT","PT_PJS"
    )), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2004,
    month = "November"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Ayu_Seccion_2007.xlsx" (sheet "Sheet1"), drop rows with blank municipality
################################################################################
df_tlax <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2007.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()
names(df_tlax) <- gsub("[- ]", "", names(df_tlax))
# drop if municipality==""
df_tlax <- df_tlax %>%
  filter(municipality != "")

################################################################################
# 2) Move coalition columns CC1_PRI_PVEM, CC2_PRI_PVEM into PRI_PVEM if missing
################################################################################

# count if CC1_PRI_PVEM!=. => in R, let's see how many are non-NA
# replace PRI_PVEM = CC1_PRI_PVEM if PRI_PVEM==. & CC1_PRI_PVEM!=.
if ("CC1_PRI_PVEM" %in% names(df_tlax)) {
  # number of non-NA
  cat("Non-NA in CC1_PRI_PVEM:", sum(!is.na(df_tlax$CC1_PRI_PVEM)), "\n")
  
  df_tlax <- df_tlax %>%
    mutate(
      PRI_PVEM = if_else(is.na(PRI_PVEM) & !is.na(CC1_PRI_PVEM), CC1_PRI_PVEM, PRI_PVEM)
    ) %>%
    select(-CC1_PRI_PVEM)
}

# similarly for CC2_PRI_PVEM
if ("CC2_PRI_PVEM" %in% names(df_tlax)) {
  cat("Non-NA in CC2_PRI_PVEM:", sum(!is.na(df_tlax$CC2_PRI_PVEM)), "\n")
  
  df_tlax <- df_tlax %>%
    mutate(
      PRI_PVEM = if_else(is.na(PRI_PVEM) & !is.na(CC2_PRI_PVEM), CC2_PRI_PVEM, PRI_PVEM)
    ) %>%
    select(-CC2_PRI_PVEM)
}

################################################################################
# 3) Create total = rowtotal(...) for columns in row
################################################################################

vote_cols <- c("PRI","PAN_PAC","PRI_PVEM","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","PS","PRI_PVEM_PS","NOREGISTRADOS","EMITIDOS","VALIDOS")
df_tlax <- df_tlax %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(any_of(vote_cols)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  filter(total != 0)

################################################################################
# 4) Collapse (sum) columns, by (municipality, section)
################################################################################
# The Stata code: 
#   collapse (sum) PRI - total, by(municipality section)
# meaning sum all columns from PRI to total. We'll do a partial approach.

# We'll identify columns from PRI to total
# Or we can just specify an explicit list: c("PRI","PAN_PAC","PRI_PVEM",..., "total")
collapse_cols <- c("PRI","PAN_PAC","PRI_PVEM","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","PS","PRI_PVEM_PS",
                   "NOREGISTRADOS","EMITIDOS","VALIDOS","total")

df_collapse <- df_tlax %>%
  group_by(municipality, section) %>%
  summarise(
    across(any_of(collapse_cols), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 5) Sort by section, merge with "Listanominal2007.dta" on 'section', drop unmatched,
#    erase "Listanominal2007.dta"
################################################################################

df_collapse <- df_collapse %>%
  arrange(section)

df_ln <- read_dta("Listanominal2007.dta")

df_merge1 <- df_collapse %>%
  left_join(df_ln, by="section") %>%
  filter(!is.na(listanominal))  # drop if no match

################################################################################
# 6) gen turnout = total/listanominal, drop NOREGISTRADOS, EMITIDOS
################################################################################
df_merge1 <- df_merge1 %>%
  mutate(turnout = total / listanominal) %>%
  select(-NOREGISTRADOS, -EMITIDOS)

################################################################################
# 7) Create uniqueid=0, then replace codes for each municipality
################################################################################

df_merge1 <- df_merge1 %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"         ~ 29022,
      municipality=="AMAXAC DE GUERRERO"                  ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"     ~ 29002,
      municipality=="APIZACO"                             ~ 29005,
      municipality=="ATLANGATEPEC"                        ~ 29003,
      municipality=="ATLTZAYANCA"                         ~ 29004,
      municipality=="BENITO JUAREZ"                       ~ 29045,
      municipality=="CALPULALPAN"                         ~ 29006,
      municipality=="CHIAUTEMPAN"                         ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"             ~ 29018,
      municipality=="CUAPIAXTLA"                          ~ 29008,
      municipality=="CUAXOMULCO"                          ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"              ~ 29007,
      municipality=="EMILIANO ZAPATA"                     ~ 29046,
      municipality=="ESPANITA"                            ~ 29012,
      municipality=="HUAMANTLA"                           ~ 29013,
      municipality=="HUEYOTLIPAN"                         ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS"    ~ 29015,
      municipality=="IXTENCO"                             ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"            ~ 29048,
      municipality=="LAZARO CARDENAS"                     ~ 29047,
      municipality=="MAZATECOCHCO DE JOSE MARIA MORELOS"  ~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"             ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"       ~ 29021,
      municipality=="NATIVITAS"                           ~ 29023,
      municipality=="PANOTLA"                             ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"           ~ 29041,
      municipality=="SAN DAMIAN TEXOLOC"                  ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"           ~ 29050,
      municipality=="SAN JERONIMO ZACUALPAN"              ~ 29051,
      municipality=="SAN JOSE TEACALCO"                   ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"                 ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"            ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"                 ~ 29055,
      municipality=="SAN PABLO DEL MONTE"                 ~ 29025,
      municipality=="SANCTORUM"                           ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"                 ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"             ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"             ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"                ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"                 ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"             ~ 29060,
      municipality=="TENANCINGO"                          ~ 29027,
      municipality=="SAN LUIS TEOLOCHOLCO"                ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"             ~ 29019,
      municipality=="TEPEYANCO"                           ~ 29029,
      municipality=="TERRENATE"                           ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"             ~ 29031,
      municipality=="TETLATLAHUCA"                        ~ 29032,
      municipality=="TLAXCALA"                            ~ 29033,
      municipality=="TLAXCO"                              ~ 29034,
      municipality=="TOCATLAN"                            ~ 29035,
      municipality=="TOTOLAC"                             ~ 29036,
      municipality=="TZOMPANTEPEC"                        ~ 29038,
      municipality=="XALOZTOC"                            ~ 29039,
      municipality=="XALTOCAN"                            ~ 29040,
      municipality=="XICOHTZINCO"                         ~ 29042,
      municipality=="YAUHQUEMEHCAN"                       ~ 29043,
      municipality=="ZACATELCO"                           ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS" ~ 29037,
      TRUE                                               ~ uniqueid
    )
  )

# rename VALIDOS->valid, gen year=2007, month="November"
df_2007 <- df_merge1 %>%
  rename(valid = VALIDOS) %>%
  mutate(
    year  = 2007,
    month = "November"
  )

################################################################################
# 1) Read "Ayu_Seccion_2010.csv", rename columns, drop missing rows, parse numeric
################################################################################

df_tlax10 <- read_csv("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2010.csv", show_col_types = FALSE) %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality == "" & is.na(section)))  # drop if municipality=="" & section==.
colnames(df_tlax10) <- tolower(colnames(df_tlax10))
names(df_tlax10) <- gsub("[- ]", "", names(df_tlax10))
# parse columns from panpna to total as numeric
# In Stata: destring panpna - total , replace
# We'll identify them explicitly or do a partial approach:
vote_cols <- c("panpna","pripvem","pri","prips","prdpc","prdpcpt","prdpcpt2",
               "prdpt","prdpt2","ptpc","pt","convergencia","pvem","pac","pp","plt","ppt","ps","nulos")

df_tlax10 <- df_tlax10 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

# drop if total ==. or total==0
df_tlax10 <- df_tlax10 %>%
  filter(!(is.na(total) | total == 0))

################################################################################
# 2) collapse (sum) listanominal panpna - total, by(municipality section)
################################################################################

# We'll find the columns from panpna to total plus listanominal:
collapse_cols <- c("listanominal","panpna","pripvem","pri","prips","prdpc","prdpcpt","prdpcpt2","prdpt","prdpt2","ptpc","pt",
                   "convergencia","pvem","pac","pp","plt","ppt","ps","nulos","total")

df_collapsed <- df_tlax10 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(intersect(collapse_cols, names(.))), sum, na.rm=TRUE), .groups="drop")

################################################################################
# 3) Coalition logic: referencing various "sum" commands and replacements in Stata
################################################################################

# * We'll replicate the logic from your code:
# sum pripvem pri pvem if pripvem!=0
# replace pvem=0 if municipality=="NATIVITAS"
# replace pri=0  if municipality=="TERRENATE"
df_collapsed <- df_collapsed %>%
  mutate(
    pvem = if_else(municipality=="NATIVITAS", 0, pvem),
    pri  = if_else(municipality=="TERRENATE", 0, pri)
  )

# sum pripvemps pri pvem ps if pripvemps!=0 => drop pripvemps
df_collapsed <- df_collapsed %>%
  select(-any_of("pripvemps"))

# sum prips pri ps if prips!=0 => no direct replacement in code except that we keep them

# sum prdpc prd convergencia if prdpc!=0 => no direct replacement

# sum prdpcpt prd convergencia pt if prdpcpt!=0
# replace prdpcpt= prdpcpt + pt  if prdpcpt!=0
# replace pt=0 if prdpcpt!=0
df_collapsed <- df_collapsed %>%
  mutate(
    prdpcpt = if_else(prdpcpt!=0, prdpcpt + coalesce(pt,0), prdpcpt),
    pt      = if_else(prdpcpt!=0, 0, pt)
  )

# sum prdpcpt2 prd convergencia pt if prdpcpt2!=0
# replace prd=0 if prdpcpt2!=0
df_collapsed <- df_collapsed %>%
  mutate(
    prd = if_else(prdpcpt2!=0, 0, prd)
  )

# sum prdpt prd pt if prdpt!=0 => no replacement
# sum prdpt2 prd pt if prdpt2!=0 => no direct replacement

# sum ptpc pt convergencia if ptpc!=0 => "They are mistakes, no effect"
# replace pt=0 if ptpc!=0
df_collapsed <- df_collapsed %>%
  mutate(
    pt = if_else(ptpc!=0, 0, pt)
  )

# count if prdpcpt!=0 & prdpcpt2!=0
# replace prdpcpt= prdpcpt2 if prdpcpt==0 & prdpcpt2!=0
# drop prdpcpt2
df_collapsed <- df_collapsed %>%
  mutate(
    prdpcpt = if_else(prdpcpt==0 & prdpcpt2!=0, prdpcpt2, prdpcpt)
  ) %>%
  select(-any_of("prdpcpt2"))

# count if prdpt!=0 & prdpt2!=0
# replace prdpt= prdpt2 if prdpt==0 & prdpt2!=0
# drop prdpt2
df_collapsed <- df_collapsed %>%
  mutate(
    prdpt = if_else(prdpt==0 & prdpt2!=0, prdpt2, prdpt)
  ) %>%
  select(-any_of("prdpt2"))

################################################################################
# 4) rename columns: panpna->PAN_PANAL, pri->PRI, pripvem->PRI_PVEM, ...
################################################################################

df_final <- df_collapsed %>%
  rename(
    PAN_PANAL = panpna,
    PRI       = pri,
    PRI_PVEM  = pripvem,
    PRI_PST   = prips,
    PRD       = prd,
    PRD_PT_PC = prdpcpt,
    PRD_PC    = prdpc,
    PRD_PT    = prdpt,
    PT_PC     = ptpc,
    PT        = pt,
    PC        = convergencia,
    PVEM      = pvem,
    PAC       = pac,
    PP        = pp,
    PLT       = plt,
    PPT       = ppt,
    PST       = ps
  )

# gen turnout= total/listanominal
df_final <- df_final %>%
  mutate(turnout = total / listanominal)

# drop nulos if it exists
df_final <- df_final %>%
  select(-any_of("nulos"))

################################################################################
# 5) Create uniqueid=0, then case_when for each municipality
################################################################################
df_final <- df_final %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"         ~ 29022,
      municipality=="AMAXAC DE GUERRERO"                  ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"     ~ 29002,
      municipality=="APIZACO"                             ~ 29005,
      municipality=="ATLANGATEPEC"                        ~ 29003,
      municipality=="ATLTZAYANCA"                         ~ 29004,
      municipality=="BENITO JUAREZ"                       ~ 29045,
      municipality=="CALPULALPAN"                         ~ 29006,
      municipality=="CHIAUTEMPAN"                         ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"             ~ 29018,
      municipality=="CUAPIAXTLA"                          ~ 29008,
      municipality=="CUAXOMULCO"                          ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"              ~ 29007,
      municipality=="EMILIANO ZAPATA"                     ~ 29046,
      municipality=="ESPANITA"                            ~ 29012,
      municipality=="HUAMANTLA"                           ~ 29013,
      municipality=="HUEYOTLIPAN"                         ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS"    ~ 29015,
      municipality=="IXTENCO"                             ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"            ~ 29048,
      municipality=="LAZARO CARDENAS"                     ~ 29047,
      municipality=="MAZATECOCHCO DE JOSE MARIA MORELOSS" ~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"             ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"       ~ 29021,
      municipality=="NATIVITAS"                           ~ 29023,
      municipality=="PANOTLA"                             ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"           ~ 29041,
      municipality=="SAN DAMIAN TEXOLOC"                  ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"           ~ 29050,
      municipality=="SAN JERONIMO ZACUALPAN"              ~ 29051,
      municipality=="SAN JOSE TEACALCO"                   ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"                 ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"            ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"                 ~ 29055,
      municipality=="SAN PABLO DEL MONTE"                 ~ 29025,
      municipality=="SANCTORUM DE LAZARO CARDENAS"        ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"                 ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"             ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"             ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"                ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"                 ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"             ~ 29060,
      municipality=="TENANCINGO"                          ~ 29027,
      municipality=="TEOLOCHOLCO"                         ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"             ~ 29019,
      municipality=="TEPEYANCO"                           ~ 29029,
      municipality=="TERRENATE"                           ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"             ~ 29031,
      municipality=="TETLATLAHUCA"                        ~ 29032,
      municipality=="TLAXCALA"                            ~ 29033,
      municipality=="TLAXCO"                              ~ 29034,
      municipality=="TOCATLAN"                            ~ 29035,
      municipality=="TOTOLAC"                             ~ 29036,
      municipality=="TZOMPANTEPEC"                        ~ 29038,
      municipality=="XALOZTOC"                            ~ 29039,
      municipality=="XALTOCAN"                            ~ 29040,
      municipality=="XICOHTZINCO"                         ~ 29042,
      municipality=="YAUHQUEMEHCAN"                       ~ 29043,
      municipality=="ZACATELCO"                           ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS" ~ 29037,
      TRUE ~ uniqueid
    )
  )

# compute valid = rowtotal( PAN_PANAL PRI PRI_PVEM PRI_PST PRD PRD_PT_PC PRD_PC PRD_PT PT PT_PC PVEM PC PST PAC PP PLT PPT )
# gen year=2010, month="July"
df_2010 <- df_final %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(c(
      "PAN_PANAL","PRI","PRI_PVEM","PRI_PST","PRD","PRD_PT_PC","PRD_PC","PRD_PT","PT",
      "PT_PC","PVEM","PC","PST","PAC","PP","PLT","PPT"
    ))), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2010,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# Part A: Tlaxcala 2013 Regular Election (July)
################################################################################

# 1) Read "Ayu_Seccion_2013.xlsx" (sheet "Sheet1"), drop blank municipalities
df_july2013 <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2013.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

names(df_july2013) <- gsub("[- ]", "", names(df_july2013))

df_july2013 <- df_july2013 %>%
  filter(municipality != "")

# 2) Rename columns: Emitidos->total, Validos->valid
df_july2013 <- df_july2013 %>%
  rename(
    total = Emitidos,
    valid = Validos
  )

# 3) Collapse (sum) from PAN through 'valid', by (municipality, section, Coalition)
#    In Stata: collapse (sum) PAN - valid, by(municipality section Coalition)
vote_cols <- names(df_july2013)
# We'll identify the columns from "PAN" to "valid" if needed, or do an explicit list
# For clarity, let's do an explicit approach if known.
collapse_cols <- c("PAN","PAC","PS","PRI","PRD","PT","PVEM","PC","PCDT","PANAL","PAS","VALID",
                   "Coalition","NoRegistrados","Emitidos","total") # adapt if needed
# But your actual column set might differ; adjust as necessary.

df_collapsed <- df_july2013 %>%
  group_by(municipality, section, Coalition) %>%
  summarise(
    across(where(is.numeric), sum, na.rm=TRUE),
    .groups="drop"
  )
# 4) If Coalition=="PAC_PS", "PAN_PAC", "PRD_PT", "PRI_PVEM", "PT_PAC" => add sums, zero out old columns, then drop Coalition
df_collapsed <- df_collapsed %>%
  mutate(
    PAC_PS = if_else(Coalition=="PAC_PS", coalesce(PAC,0) + coalesce(PS,0), NA_real_),
    PAC = if_else(Coalition=="PAC_PS", 0, PAC),
    PS  = if_else(Coalition=="PAC_PS", 0, PS),
    
    PAN_PAC = if_else(Coalition=="PAN_PAC", coalesce(PAN,0)+coalesce(PAC,0), NA_real_),
    PAN = if_else(Coalition=="PAN_PAC", 0, PAN),
    PAC = if_else(Coalition=="PAN_PAC", 0, PAC),
    
    PRD_PT = if_else(Coalition=="PRD_PT", coalesce(PRD,0)+coalesce(PT,0), NA_real_),
    PRD = if_else(Coalition=="PRD_PT", 0, PRD),
    PT  = if_else(Coalition=="PRD_PT", 0, PT),
    
    PRI_PVEM = if_else(Coalition=="PRI_PVEM", coalesce(PRI,0)+coalesce(PVEM,0), NA_real_),
    PRI  = if_else(Coalition=="PRI_PVEM", 0, PRI),
    PVEM = if_else(Coalition=="PRI_PVEM", 0, PVEM),
    
    PT_PAC = if_else(Coalition=="PT_PAC", coalesce(PT,0)+coalesce(PAC,0), NA_real_),
    PT  = if_else(Coalition=="PT_PAC", 0, PT),
    PAC = if_else(Coalition=="PT_PAC", 0, PAC)
  ) %>%
  select(-Coalition)

# 5) Sort by section, merge with "Listanominal2013.dta" on `section`, drop unmatched
df_collapsed <- df_collapsed %>%
  arrange(section)

df_ln2013 <- read_dta("Listanominal2013.dta")
df_merged <- df_collapsed %>%
  left_join(df_ln2013, by="section") %>%
  filter(!is.na(listanominal))

# erase "Listanominal2013.dta"
file.remove("Listanominal2013.dta")

# 6) gen turnout= total/listanominal, drop NoRegistrados
df_merged <- df_merged %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-any_of("NoRegistrados"))

# 7) Convert municipality to uppercase, assign uniqueid
df_merged <- df_merged %>%
  mutate(
    municipality = toupper(municipality),
    uniqueid = 0
  )

df_2013 <- df_merged %>%
  mutate(
    uniqueid = case_when(
      municipality=="ACUAMANALA DE MIGUEL HIDALGO"        ~ 29022,
      municipality=="AMAXAC DE GUERRERO"                 ~ 29001,
      municipality=="APETATITLAN DE ANTONIO CARVAJAL"    ~ 29002,
      municipality=="APIZACO"                            ~ 29005,
      municipality=="ATLANGATEPEC"                       ~ 29003,
      municipality=="ATLTZAYANCA"                        ~ 29004,
      municipality=="BENITO JUAREZ"                      ~ 29045,
      municipality=="CALPULALPAN"                        ~ 29006,
      municipality=="CHIAUTEMPAN"                        ~ 29010,
      municipality=="CONTLA DE JUAN CUAMATZI"            ~ 29018,
      municipality=="CUAPIAXTLA"                         ~ 29008,
      municipality=="CUAXOMULCO"                         ~ 29009,
      municipality=="EL CARMEN TEQUEXQUITLA"             ~ 29007,
      municipality=="EMILIANO ZAPATA"                    ~ 29046,
      municipality=="ESPANITA"                           ~ 29012,
      municipality=="HUAMANTLA"                          ~ 29013,
      municipality=="HUEYOTLIPAN"                        ~ 29014,
      municipality=="IXTACUIXTLA DE MARIANO MATAMOROS"   ~ 29015,
      municipality=="IXTENCO"                            ~ 29016,
      municipality=="LA MAGDALENA TLALTELULCO"           ~ 29048,
      municipality=="LAZARO CARDENAS"                    ~ 29047,
      municipality=="MAZATECOCHCO DE JOSE MARIA MORELOS" ~ 29017,
      municipality=="MUNOZ DE DOMINGO ARENAS"            ~ 29011,
      municipality=="NANACAMILPA DE MARIANO ARISTA"      ~ 29021,
      municipality=="NATIVITAS"                          ~ 29023,
      municipality=="PANOTLA"                            ~ 29024,
      municipality=="PAPALOTLA DE XICOHTENCATL"          ~ 29041,
      municipality=="SAN DAMIAN TEXOLOC"                 ~ 29049,
      municipality=="SAN FRANCISCO TETLANOHCAN"          ~ 29050,
      municipality=="SAN JERONIMO ZACUALPAN"             ~ 29051,
      municipality=="SAN JOSE TEACALCO"                  ~ 29052,
      municipality=="SAN JUAN HUACTZINCO"                ~ 29053,
      municipality=="SAN LORENZO AXOCOMANITLA"           ~ 29054,
      municipality=="SAN LUCAS TECOPILCO"                ~ 29055,
      municipality=="SAN PABLO DEL MONTE"                ~ 29025,
      municipality=="SANCTORUM DE LAZARO CARDENAS"       ~ 29020,
      municipality=="SANTA ANA NOPALUCAN"                ~ 29056,
      municipality=="SANTA APOLONIA TEACALCO"            ~ 29057,
      municipality=="SANTA CATARINA AYOMETLA"            ~ 29058,
      municipality=="SANTA CRUZ QUILEHTLA"               ~ 29059,
      municipality=="SANTA CRUZ TLAXCALA"                ~ 29026,
      municipality=="SANTA ISABEL XILOXOXTLA"            ~ 29060,
      municipality=="TENANCINGO"                         ~ 29027,
      municipality=="TEOLOCHOLCO"                        ~ 29028,
      municipality=="TEPETITLA DE LARDIZABAL"            ~ 29019,
      municipality=="TEPEYANCO"                          ~ 29029,
      municipality=="TERRENATE"                          ~ 29030,
      municipality=="TETLA DE LA SOLIDARIDAD"            ~ 29031,
      municipality=="TETLATLAHUCA"                       ~ 29032,
      municipality=="TLAXCALA"                           ~ 29033,
      municipality=="TLAXCO"                             ~ 29034,
      municipality=="TOCATLAN"                           ~ 29035,
      municipality=="TOTOLAC"                            ~ 29036,
      municipality=="TZOMPANTEPEC"                       ~ 29038,
      municipality=="XALOZTOC"                           ~ 29039,
      municipality=="XALTOCAN"                           ~ 29040,
      municipality=="XICOHTZINCO"                        ~ 29042,
      municipality=="YAUHQUEMEHCAN"                      ~ 29043,
      municipality=="ZACATELCO"                          ~ 29044,
      municipality=="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS" ~ 29037,
      TRUE ~ uniqueid
    )
  ) %>%
  mutate(
    year  = 2013,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# Part B: Tlaxcala 2013 Extraordinario (December)
################################################################################

# 1) Read "Resultados Extraordinaria 8 Diciembre 2013.xlsx" (sheet "Sheet1"), rename columns
df_extra <- read_excel(
  path = "Resultados Extraordinaria 8 Diciembre 2013.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

df_extra <- df_extra %>%
  rename(
    section = Sección,
    total   = EMIT,
    valid   = VALID,
    PVEM    = VERDE
  ) %>%
  mutate(uniqueid = 29002)  # from the code: g uniqueid=29002

# 2) collapse (sum) columns from "PAN" to "PS" plus "total" and "valid", by(municipality uniqueid section)
vote_cols2 <- c("PAN","PRI","PRD","PT","PVEM","MC","PANAL","PAC","PS","PC","PartidoX",   # adapt as needed
                "total","valid")

df_extra_collapse <- df_extra %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(any_of(vote_cols2), sum, na.rm=TRUE), .groups="drop")

# 3) Merge with "all_months_years.dta" for (ed=29, month=11, year=2013)? The code:
#    preserve
#    use "..\..\all_months_years.dta", clear
#    keep if ed==29 & month==11 & year==2013
#    rename seccion->section, rename lista->listanominal
#    save "merge.dta", restore
#    merge 1:1 section using "merge.dta", keepusing(listanominal)
#    drop if _merge==2
#    drop _merge
#    erase "merge.dta"

df_allm <- read_dta("../../all_months_years.dta") %>%
  filter(ed==29, month==11, year==2013) %>%
  rename(section=seccion, listanominal=lista) %>%
  select(section, listanominal)

df_extra_merged <- df_extra_collapse %>%
  left_join(df_allm, by="section") %>%
  filter(!is.na(listanominal))

# remove "merge.dta" if it existed (in R, we won't create it unless we replicate exactly)

# 4) g turnout= total/listanominal, gen year=2013, gen month="December"
df_extra_2013 <- df_extra_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2013,
    month   = "December"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Resultados Extraordinaria 23 Febrero 2014.xlsx" from range(A6:K13),
#    rename columns, set uniqueid & municipality
################################################################################

df_2014_extra <- read_excel(
  path     = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Resultados Extraordinaria 23 Febrero 2014.xlsx",
  sheet    = "Hoja1",
  range    = "A6:K13",
  col_names= TRUE
) %>%
  as.data.frame()
names(df_2014_extra)
# rename Sección->section, EMIT->total, VALID->valid
df_2014_extra <- df_2014_extra %>%
  rename(
    section = Sección,
    total   = EMITIDOS,
    valid   = VALIDOS
  ) %>%
  mutate(
    uniqueid    = 29022,
    municipality= "ACUAMANALA DE MIGUEL HIDALGO EXTRAORDINARIO"
  )

################################################################################
# 2) collapse (sum) from PAN to MC (plus total and valid) 
#    by (municipality, uniqueid, section)
################################################################################

# Identify columns from "PAN" to "MC" plus "total" and "valid".
# For example, if your columns are named exactly PAN, PRI, PRD, PT, ... , MC, total, valid.
# We'll specify them explicitly for clarity:
collapse_cols <- c("PAN","PRI","PRD","PT","PVEM","MC","total","valid")

df_collapsed <- df_2014_extra %>%
  group_by(municipality, uniqueid, section) %>%
  summarise(across(all_of(intersect(collapse_cols, names(.))), sum, na.rm=TRUE),
            .groups="drop")

################################################################################
# 3) Merging with all_months_years.dta for (ed=29, month=1, year=2014)
################################################################################

df_allm <- read_dta("../../all_months_years.dta") %>%
  filter(ed==29, month==1, year==2014) %>%
  rename(section=seccion, listanominal=lista) %>%
  select(section, listanominal)

df_merged <- df_collapsed %>%
  left_join(df_allm, by="section") %>%
  filter(!is.na(listanominal))  # dropping unmatched

################################################################################
# 4) Compute turnout= total/listanominal, set year=2014, month="February"
################################################################################

df_2014 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2014,
    month   = "February"
  ) %>%
  arrange(section)

################################################################################
# Part A: Reading each sheet from "Ayuntamientos_Tlaxcala_2016.xlsx" and
#         saving as .dta
################################################################################

all_sheets <- excel_sheets("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayuntamientos_Tlaxcala_2016.xlsx")

for (sheetname in all_sheets) {
  df_sheet <- read_excel(
    path = "../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/Ayuntamientos_Tlaxcala_2016.xlsx",
    sheet = sheetname,
    col_names = TRUE,
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # drop if section==""
  if ("section" %in% names(df_sheet)) {
    df_sheet <- df_sheet %>%
      filter(section!="")
  }
  
  # for each column, replace "" with "0"
  df_sheet <- df_sheet %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # save as `sheetname`.dta
  write_dta(df_sheet, paste0(sheetname, ".dta"))
}

################################################################################
# Part B: Appending multiple .dta files (like "Table 1.dta", "Table 2.dta", etc.)
################################################################################

append_files <- c(
  "Table 1.dta", "Table 2.dta", "Table 3.dta", "Table 4.dta", "Table 5.dta",
  "Table 7.dta", "Table 8.dta", "Table 9.dta", "Table 11.dta", "Table 13.dta",
  "Table 15.dta", "Table 16.dta", "Table 17.dta", "Table 18.dta", "Table 19.dta",
  "Table 20.dta", "Table 22.dta", "Table 23.dta", "Table 25.dta", "Table 26.dta",
  "Table 27.dta", "Table 28.dta", "Table 29.dta", "Table 30.dta", "Table 31.dta",
  "Table 32.dta", "Table 33.dta", "Table 34.dta", "Table 35.dta", "Table 36.dta",
  "Table 37.dta", "Table 38.dta", "Table 39.dta", "Table 40.dta", "Table 41.dta",
  "Table 43.dta", "Table 44.dta", "Table 45.dta", "Table 46.dta", "Table 47.dta",
  "Table 48.dta", "Table 49.dta", "Table 50.dta", "Table 51.dta", "Table 52.dta",
  "Table 53.dta", "Table 54.dta", "Table 55.dta", "Table 56.dta", "Table 57.dta",
  "Table 59.dta", "Table 61.dta", "Table 62.dta", "Table 63.dta", "Table 64.dta",
  "Table 65.dta", "Table 66.dta", "Table 67.dta", "Table 68.dta", "Table 70.dta"
)

df_all <- data.frame()

for (f in append_files) {
  if (file.exists(f)) {
    df_new <- read_dta(f)
    df_all <- bind_rows(df_all, df_new)
  }
}

################################################################################
# Part C: Data cleaning after append
################################################################################

# drop P Q R S T if exist
df_all <- df_all %>%
  select(-any_of(c("P","Q","R","S","T")))

# replace municipality=upper(municipality)
if ("municipality" %in% names(df_all)) {
  df_all <- df_all %>%
    mutate(municipality = toupper(municipality))
}

# destring *, replace => in R, parse columns as numeric if they are characters:
df_all <- df_all %>%
  mutate(across(PAN:PVEM_PS, ~ suppressWarnings(as.numeric(.))))

# drop if strpos(municipality,"TOTAL")>0 or municipality==""
df_all <- df_all %>%
  filter(!str_detect(municipality, "TOTAL")) %>%
  filter(municipality != "")

# collapse (sum) by (municipality, section): columns from PAN to PVEM_PS
collapse_cols2 <- names(df_all)
# We'll guess you want from "PAN" through "PVEM_PS". Adapt as needed.
# In Stata: collapse (sum) PAN-PVEM_PS, by(municipality section)

df_collapsed <- df_all %>%
  group_by(municipality, section) %>%
  summarise(across(PAN:PVEM_PS, ~ sum(., na.rm=TRUE)), .groups="drop")

################################################################################
# Part D: Merge with uniqueids16.dta
################################################################################

# preserve
df_uniqueids <- read_excel("../../../Data/Raw Electoral Data/Tlaxcala 2001, 2004, 2007, 2010, 2013,2016/uniqueids16.xlsx", col_names=TRUE) %>%
  as.data.frame()

df_merge_ids <- df_collapsed %>%
  left_join(df_uniqueids, by="municipality") %>%
  select(-starts_with("_merge"))


# drop municipality, rename municipio->municipality (if you have 'municipio' in df_ids)
if ("municipio" %in% names(df_merge_ids)) {
  df_merge_ids <- df_merge_ids %>%
    select(-municipality) %>%
    rename(municipality = municipio)
}

# order municipality uniqueid section *
col_order <- c("municipality","uniqueid","section")
df_merge_ids <- df_merge_ids %>%
  select(all_of(col_order), everything())

# drop validos total => if they exist
df_merge_ids <- df_merge_ids %>%
  select(-any_of(c("validos","total")))

# order CI_* after PVEM_PS => we can do a custom reorder if needed:
# order no_reg nulo, a(CI_4)

################################################################################
# Part E: Compute valid=..., total= valid + no_reg + nulo, remove no_reg nulo
# year=2016, month="June", STATE="TLAXCALA"
################################################################################

# We'll define which columns to row-sum for 'valid'
valid_cols <- c("PAN","PRI","PRD_PT","PVEM","MC","PANAL","PAC","PS","MORENA","PES",
                "PRD","PT","PRI_PANAL","PRI_PVEM_PANAL_PS","PRI_PVEM","PRI_PANAL_PS",
                "PRI_PS","PRI_PT","PRI_PVEM_PANAL","PVEM_PS","CI_1","CI_2","CI_3","CI_4")

df_merge_ids <- df_merge_ids %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(valid_cols)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    total = valid + coalesce(no_reg,0) + coalesce(nulo,0)
  ) %>%
  select(-any_of(c("no_reg","nulo"))) %>%
  mutate(
    year  = 2016,
    month = "June",
    STATE = "TLAXCALA"
  )

################################################################################
# Part F: Merge with LN2016.dta for listanominal, compute turnout, save
################################################################################

# preserve => in R we won't do it the same, we'll just read LN2016:
df_ln16 <- read_dta("../Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==29, month==5) %>%
  mutate(
    uniqueid = (entidad*1000) + municipio
  ) %>%
  filter(seccion!=0) %>%
  arrange(uniqueid,seccion) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section=seccion)

# We'll do a left_join on (uniqueid, section) presumably:
df_final <- df_merge_ids %>%
  left_join(df_ln16, by=c("uniqueid","section")) %>%
  filter(!is.na(lista))  # drop unmatched

# rename lista->listanominal
df_final <- df_final %>%
  rename(listanominal = lista) %>%
  mutate(
    turnout = total / listanominal
  )

################################################################################
# Part G: Appending with existing "Tlaxcala_ALL.dta" and "Tlaxcala_Section_2016.dta"
################################################################################
df_combined <- bind_rows(df_tlax_all, df_final)


# replace municipality names
df_combined <- df_combined %>%
  mutate(
    municipality = if_else(municipality=="ALTZAYANCA","ATLTZAYANCA", municipality),
    municipality = if_else(municipality=="TEOLOCHOLCO","SAN LUIS TEOLOCHOLCO", municipality)
  )

################################################################################
# Part H: erase the "Table X.dta" files
################################################################################
tables_to_erase <- c(
  "Table 1.dta", "Table 2.dta", "Table 3.dta", "Table 4.dta", "Table 5.dta",
  "Table 7.dta", "Table 8.dta", "Table 9.dta", "Table 11.dta", "Table 13.dta",
  "Table 15.dta", "Table 16.dta", "Table 17.dta", "Table 18.dta", "Table 19.dta",
  "Table 20.dta", "Table 22.dta", "Table 23.dta", "Table 25.dta", "Table 26.dta",
  "Table 27.dta", "Table 28.dta", "Table 29.dta", "Table 30.dta", "Table 31.dta",
  "Table 32.dta", "Table 33.dta", "Table 34.dta", "Table 35.dta", "Table 36.dta",
  "Table 37.dta", "Table 38.dta", "Table 39.dta", "Table 40.dta", "Table 41.dta",
  "Table 43.dta", "Table 44.dta", "Table 45.dta", "Table 46.dta", "Table 47.dta",
  "Table 48.dta", "Table 49.dta", "Table 50.dta", "Table 51.dta", "Table 52.dta",
  "Table 53.dta", "Table 54.dta", "Table 55.dta", "Table 56.dta", "Table 57.dta",
  "Table 59.dta", "Table 61.dta", "Table 62.dta", "Table 63.dta", "Table 64.dta",
  "Table 65.dta", "Table 66.dta", "Table 67.dta", "Table 68.dta", "Table 70.dta"
)

for (tf in tables_to_erase) {
  if (file.exists(tf)) {
    file.remove(tf)
  }
}


# Combine the dataframes, handling different columns by filling with NA
tlaxcala_all <- bind_rows(df_2001,
                          df_2004,
                          df_2007,
                          df_2010,
                          df_2013,
                          df_extra_2013,
                          df_2014,
                          df_final
                          )

data.table::fwrite(tlaxcala_all,"../../../Processed Data/tlaxcala/Tlaxcala_process_raw_data.csv")

