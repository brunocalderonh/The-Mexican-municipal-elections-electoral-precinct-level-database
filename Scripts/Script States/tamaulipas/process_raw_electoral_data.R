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
# 1) Read "Ayu_Seccion_1998_No_LN.csv", rename columns, drop empties, parse numeric
################################################################################

# In Stata: insheet using Ayu_Seccion_1998_No_LN.csv, clear
df_tam <- read_csv("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_1998_No_LN.csv", 
                   show_col_types=FALSE) 
colnames(df_tam) <- tolower(colnames(df_tam))
names(df_tam) <- gsub("[- ]", "", names(df_tam))


df_tam <-df_tam %>%
  rename(
    municipality = municipio,
    section      = secc
    # If there's a "nominal" column, we do not rename or we ignore, 
    # since the code has "rename listanominal nominal" commented out
  ) %>%
  # drop if municipality=="" & section==.
  filter(!(municipality=="" & is.na(section)))

# parse columns pan - total => we identify them or do partial approach:
vote_cols <- c("pan","pri","prd","pt","pvem","pc","parm","nulos","noreg","total")
df_tam <- df_tam %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))


################################################################################
# 2) collapse (sum) pan - total by (municipality, section)
################################################################################

# We'll group by municipality, section, then sum those columns
df_collapse <- df_tam %>%
  group_by(municipality, section) %>%
  summarise(
    across(any_of(c("pan","pri","prd","pt","pvem","pc","parm","nulos","noregistrados","total")),
           sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) rename columns (pan->PAN, pri->PRI, prd->PRD, etc.), drop nulos noreg
################################################################################

df_collapse <- df_collapse %>%
  rename(
    PAN            = pan,
    PRI            = pri,
    PRD            = prd,
    PT             = pt,
    PVEM           = pvem,
    PartCardenista = pc,     # rename pc -> PartCardenista
    PARM           = parm
  ) %>%
  select(-nulos, -noregistrados)

################################################################################
# 4) assign uniqueid=0, then set codes for each municipality
################################################################################

df_collapse <- df_collapse %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ABASOLO"       ~ 28001,
      municipality=="ALDAMA"        ~ 28002,
      municipality=="ALTAMIRA"      ~ 28003,
      municipality=="ANT. MORELOS"  ~ 28004,
      municipality=="BURGOS"        ~ 28005,
      municipality=="BUSTAMANTE"    ~ 28006,
      municipality=="CAMARGO"       ~ 28007,
      municipality=="CASAS"         ~ 28008,
      municipality=="CD. MADERO"    ~ 28009,
      municipality=="CRUILLAS"      ~ 28010,
      municipality=="MANTE"         ~ 28021,
      municipality=="GOMEZ FARIAS"  ~ 28011,
      municipality=="GONZALEZ"      ~ 28012,
      municipality=="GUEMEZ"        ~ 28013,
      municipality=="GUERRERO"      ~ 28014,
      municipality=="G DIAZ ORDAZ"  ~ 28015,
      municipality=="HIDALGO"       ~ 28016,
      municipality=="JAUMAVE"       ~ 28017,
      municipality=="JIMENEZ"       ~ 28018,
      municipality=="LLERA"         ~ 28019,
      municipality=="MAINERO"       ~ 28020,
      municipality=="MATAMOROS"     ~ 28022,
      municipality=="MENDEZ"        ~ 28023,
      municipality=="MIER"          ~ 28024,
      municipality=="MIGUEL ALEMAN" ~ 28025,
      municipality=="MIQUIHUANA"    ~ 28026,
      municipality=="NVO. LAREDO"   ~ 28027,
      municipality=="NVO. MORELOS"  ~ 28028,
      municipality=="OCAMPO"        ~ 28029,
      municipality=="PADILLA"       ~ 28030,
      municipality=="PALMILLA"      ~ 28031,
      municipality=="REYNOSA"       ~ 28032,
      municipality=="RIO BRAVO"     ~ 28033,
      municipality=="SAN CARLOS"    ~ 28034,
      municipality=="SAN FERNANDO"  ~ 28035,
      municipality=="SAN NICOLAS"   ~ 28036,
      municipality=="SOTO LA MARINA"~ 28037,
      municipality=="TAMPICO"       ~ 28038,
      municipality=="TULA"          ~ 28039,
      municipality=="VALLE HERMOSO" ~ 28040,
      municipality=="VICTORIA"      ~ 28041,
      municipality=="VILLAGRAN"     ~ 28042,
      municipality=="XICOTENCATL"   ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 5) compute valid= rowtotal(PAN PRI PRD PT PVEM PARM PartCardenista),x
################################################################################

df_collapse <- df_collapse %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRD","PT","PVEM","PARM","PartCardenista")), na.rm=TRUE)
  ) %>%
  ungroup()


################################################################################
# 6) Merge with "..\..\all_months_years.dta" using (ed=28, seccion=section),
#    keep if month==9 & year==1998, drop unmatched, rename lista->listanominal
################################################################################

df_collapse <- df_collapse %>%
  mutate(ed=28, seccion=section)

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista) 

df_join <- df_collapse %>%
  left_join(df_all, by=c("ed","seccion")) %>%
  filter(!is.na(lista)) %>%
  select(-ed, -seccion, -year, -month)

df_join <- df_join %>%
  rename(listanominal = lista)

################################################################################
# 7) gen turnout= total/listanominal, gen year=1998, month="November"
#    save "Tamaulipas_Section_1998.dta"
################################################################################

df_join <- df_join %>%
  mutate(
    turnout = total / listanominal,
    year    = 1998,
    month   = "November"
  )

################################################################################
# 8) keep municipality section uniqueid => save "Tamaulipas_Section_1998_to_Merge_1995.dta"
################################################################################

df_to_merge <- df_join %>%
  select(municipality, section, uniqueid) %>%
  arrange(section)

################################################################################
# 9) use "Tamaulipas_Section_1998.dta", drop if total==. or total==0, save
################################################################################

df_1998 <- df_join %>%
  filter(!(is.na(total) | total==0))


################################################################################
# 1) Read "Ayu_Seccion_1995_No_Municipalities_No_LN.csv"
################################################################################

df_1995 <- read_csv("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_1995_No_Municipalities_No_LN.csv", 
                    show_col_types=FALSE) 
colnames(df_1995) <- tolower(colnames(df_1995))
names(df_1995) <- gsub("[- ]", "", names(df_1995))

df_1995 <-df_1995 %>%
  rename(section = seccion) %>%
  filter(!is.na(section)) %>%          # drop if section==.
  filter(!(is.na(total) | total==0))   # drop if total==. | total==0

# parse columns from 'pan' to 'total' as numeric
vote_cols <- c("pan","pri","prd","pt","pvem","pfcrn","parm","nulos","validos","total")

df_1995 <- df_1995 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) collapse (sum) pan - total by (section)
################################################################################

# We'll group by `section` and sum the relevant columns (pan through total).
df_collapse <- df_1995 %>%
  group_by(section) %>%
  summarise(
    across(any_of(c("pan","pri","prd","pt","pvem","pfcrn","parm","nulos","validos","total")),
           sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) rename columns, drop 'nulos', 'validos'
################################################################################

df_collapse <- df_collapse %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PT              = pt,
    PVEM            = pvem,
    PartCardenista  = pfcrn,  # rename pfcrn->PartCardenista
    PARM            = parm
  ) %>%
  select(-nulos, -validos)

################################################################################
# 4) sort by section, merge with "Tamaulipas_Section_1998_to_Merge_1995.dta"
################################################################################

df_collapse <- df_collapse %>%
  arrange(section)

# We'll do a left_join on 'section'
df_merged <- df_collapse %>%
  left_join(df_to_merge, by="section") %>%
  # drop if unmatched
  filter(!is.na(municipality) & !is.na(uniqueid))

################################################################################
# 5) compute valid= rowtotal(PAN PRI PRD PartCardenista PARM PT PVEM), 
#    year=1995, month="November"
#    save "Tamaulipas_Section_1995.dta"
################################################################################

df_1995 <- df_merged %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRD","PartCardenista","PARM","PT","PVEM")), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 1995,
    month = "November"
  )

################################################################################
# 1) Read "Ayu_Seccion_2001_No_LN.csv", rename columns, drop incomplete, parse numeric
################################################################################

df_2001 <- read_csv("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2001_No_LN.csv", 
                    show_col_types=FALSE) 

colnames(df_2001) <- tolower(colnames(df_2001))
names(df_2001) <- gsub("[- ]", "", names(df_2001))

df_2001 <-df_2001 %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  # drop if municipality=="" & section==.
  filter(!(municipality=="" & is.na(section)))

# drop if total==. or total==0
df_2001 <- df_2001 %>%
  filter(!is.na(total) & total!=0)

# destring pan - total => parse numeric
vote_cols <- c("pan","pri","prd","pt","pvem","pc","pas","psn","ptpvem","paspvempt",
               "prdpvem","nulos","validos","total")
df_2001 <- df_2001 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) collapse (sum) pan - total , by(municipality, section)
################################################################################

df_collapse <- df_2001 %>%
  group_by(municipality, section) %>%
  summarise(
    across(any_of(vote_cols), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) Drop 'prdpvem', fix special coalitions for MANTE (PT-PVEM) and REYNOSA (PAS-PVEM-PT)
################################################################################

df_collapse <- df_collapse %>%
  select(-any_of("prdpvem"))

# * MANTE: PT-PVEM
df_collapse <- df_collapse %>%
  mutate(
    ptpvem = if_else(municipality=="MANTE", ptpvem + coalesce(pt,0) + coalesce(pvem,0), ptpvem),
    pt     = if_else(municipality=="MANTE", 0, pt),
    pvem   = if_else(municipality=="MANTE", 0, pvem)
  )

# * REYNOSA: PAS-PVEM-PT
df_collapse <- df_collapse %>%
  mutate(
    paspvempt = if_else(municipality=="REYNOSA",
                        paspvempt + coalesce(pas,0) + coalesce(pvem,0) + coalesce(pt,0),
                        paspvempt),
    pas  = if_else(municipality=="REYNOSA", 0, pas),
    pvem = if_else(municipality=="REYNOSA", 0, pvem),
    pt   = if_else(municipality=="REYNOSA", 0, pt)
  )

################################################################################
# 4) replace total=. if total==0 (in Stata code)
################################################################################

df_collapse <- df_collapse %>%
  mutate(total = if_else(total==0, NA_real_, total))

################################################################################
# 5) rename columns
################################################################################

df_collapse <- df_collapse %>%
  rename(
    PAN             = pan,
    PRI             = pri,
    PRD             = prd,
    PT              = pt,
    PVEM            = pvem,
    PC              = pc,   # rename pc->PC
    PAS             = pas,
    PSN             = psn,
    PT_PVEM         = ptpvem,
    PAS_PT_PVEM     = paspvempt
  )

# drop validos, nulos
df_collapse <- df_collapse %>%
  select(-validos, -nulos)

################################################################################
# 6) Create uniqueid=0, then assign codes by municipality
################################################################################

df_collapse <- df_collapse %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ABASOLO"         ~ 28001,
      municipality=="ALDAMA"          ~ 28002,
      municipality=="ALTAMIRA"        ~ 28003,
      municipality=="A. MORELOS"      ~ 28004,
      municipality=="BURGOS"          ~ 28005,
      municipality=="BUSTAMANTE"      ~ 28006,
      municipality=="CAMARGO"         ~ 28007,
      municipality=="CASAS"           ~ 28008,
      municipality=="CD. MADERO"      ~ 28009,
      municipality=="CRUILLAS"        ~ 28010,
      municipality=="MANTE"           ~ 28021,
      municipality=="GOMEZ F."        ~ 28011,
      municipality=="GONZALEZ"        ~ 28012,
      municipality=="GUEMEZ"          ~ 28013,
      municipality=="GUERRERO"        ~ 28014,
      municipality=="GVO. DIAZ O."    ~ 28015,
      municipality=="HIDALGO"         ~ 28016,
      municipality=="JAUMAVE"         ~ 28017,
      municipality=="JIMENEZ"         ~ 28018,
      municipality=="LLERA"           ~ 28019,
      municipality=="MAINERO"         ~ 28020,
      municipality=="MATAMOROS"       ~ 28022,
      municipality=="MENDEZ"          ~ 28023,
      municipality=="MIER"            ~ 28024,
      municipality=="MIGUEL A."       ~ 28025,
      municipality=="MIQUIHUANA"      ~ 28026,
      municipality=="NVO. LAREDO"     ~ 28027,
      municipality=="NVO. MORELOS"    ~ 28028,
      municipality=="OCAMPO"          ~ 28029,
      municipality=="PADILLA"         ~ 28030,
      municipality=="PALMILLAS"       ~ 28031,
      municipality=="REYNOSA"         ~ 28032,
      municipality=="RíO BRAVO"       ~ 28033,
      # or handle "RIO BRAVO" partial: if municipality=="RíO BRAVO" or something with a partial
      municipality== "RIO BRAVO" | str_detect(municipality, "O BRAVO") ~ 28033,
      municipality=="SAN CARLOS"      ~ 28034,
      municipality=="SAN FERNANDO"    ~ 28035,
      municipality=="SAN NICOLAS"     ~ 28036,
      municipality=="SOTO LA MARINA"  ~ 28037,
      municipality=="TAMPICO"         ~ 28038,
      municipality=="TULA"            ~ 28039,
      municipality=="VALLE HERMOSO"   ~ 28040,
      municipality=="VICTORIA"        ~ 28041,
      municipality=="VILLAGRAN"       ~ 28042,
      municipality=="XICOTENCATL"     ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 7) Merge with "..\..\all_months_years.dta" for ed=28, seccion=section,
#    keep if (month==9 & year==2001), drop unmatched, rename lista->listanominal
################################################################################

df_collapse <- df_collapse %>%
  mutate(ed=28, seccion=section)

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

df_merged <- df_collapse %>%
  left_join(df_all, by=c("ed","seccion")) %>%
  filter(month==9, year==2001) %>%
  filter(!is.na(lista)) %>%
  select(-ed, -seccion, -year, -month)

df_merged <- df_merged %>%
  rename(listanominal=lista)

################################################################################
# 8) Compute turnout= total/listanominal, gen year=2001, month="October",
################################################################################

df_2001 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2001,
    month   = "October"
  )

################################################################################
# 1) Read Excel "Ayu_Seccion_2004_No_LN.xlsx", rename columns, drop incomplete,
#    parse numeric columns
################################################################################

df_2004 <- read_excel(
  path     = "../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2004_No_LN.xlsx",
  sheet    = 1,       # or specify the correct sheet if not the first
  col_names= TRUE
) %>%
  as.data.frame()
colnames(df_2004) <- tolower(colnames(df_2004))
names(df_2004) <- gsub("[- ]", "", names(df_2004))
# rename municipio->municipality, seccion->section
df_2004 <- df_2004 %>%
  rename(
    municipality = municipio,
    section      = seccion
  )

# drop if municipality=="" & section==.
df_2004 <- df_2004 %>%
  filter(!(municipality=="" & is.na(section)))

# drop if total==. or total==0
df_2004 <- df_2004 %>%
  filter(!is.na(total) & total!=0)

# destring pan - total => parse numeric columns
vote_cols <- c("pan","pri","prdpc","pt","pvem","nulos","validos","total")
df_2004 <- df_2004 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) collapse (sum) pan - total by (municipality, section)
################################################################################

df_collapsed <- df_2004 %>%
  group_by(municipality, section) %>%
  summarise(
    across(any_of(vote_cols), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) rename columns, drop validos, nulos
################################################################################

df_collapsed <- df_collapsed %>%
  rename(
    PAN    = pan,
    PRI    = pri,
    PRD_PC = prdpc,
    PT     = pt,
    PVEM   = pvem
  ) %>%
  select(-validos, -nulos)

################################################################################
# 4) create uniqueid=0, then assign municipality codes
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ABASOLO"           ~ 28001,
      municipality=="ALDAMA"            ~ 28002,
      municipality=="ALTAMIRA"          ~ 28003,
      municipality=="ANTIGUO MORELOS"   ~ 28004,
      municipality=="BURGOS"            ~ 28005,
      municipality=="BUSTAMANTE"        ~ 28006,
      municipality=="CAMARGO"           ~ 28007,
      municipality=="CASAS"             ~ 28008,
      municipality=="CD. MADERO"        ~ 28009,
      municipality=="CRUILLAS"          ~ 28010,
      municipality=="MANTE"             ~ 28021,
      municipality=="GÓMEZ FARIAS"      ~ 28011,
      municipality=="GONZÁLEZ"          ~ 28012,
      municipality=="GÜÉMEZ"            ~ 28013,
      municipality=="GUERRERO"          ~ 28014,
      municipality=="G. DIAZ ORDAZ"     ~ 28015,
      municipality=="HIDALGO"           ~ 28016,
      municipality=="JAUMAVE"           ~ 28017,
      municipality=="JIMÉNEZ"           ~ 28018,
      municipality=="LLERA"             ~ 28019,
      municipality=="MAINERO"           ~ 28020,
      municipality=="MATAMOROS"         ~ 28022,
      municipality=="MENDEZ"            ~ 28023,
      municipality=="MIER"              ~ 28024,
      municipality=="MIGUEL ALEMAN"     ~ 28025,
      municipality=="MIQUIHUANA"        ~ 28026,
      municipality=="NUEVO LAREDO"      ~ 28027,
      municipality=="NUEVO MORELOS"     ~ 28028,
      municipality=="OCAMPO"            ~ 28029,
      municipality=="PADILLA"           ~ 28030,
      municipality=="PALMILLAS"         ~ 28031,
      municipality=="REYNOSA"           ~ 28032,
      municipality=="RIO BRAVO"         ~ 28033,
      municipality=="SAN CARLOS"        ~ 28034,
      municipality=="SAN FERNANDO"      ~ 28035,
      municipality=="SAN NICOLAS"       ~ 28036,
      municipality=="SOTO LA MARINA"    ~ 28037,
      municipality=="TAMPICO"           ~ 28038,
      municipality=="TULA"              ~ 28039,
      municipality=="VALLE HERMOSO"     ~ 28040,
      municipality=="VICTORIA"          ~ 28041,
      municipality=="VILLAGRAN"         ~ 28042,
      municipality=="XICOTENCATL"       ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 5) compute valid= rowtotal(PAN PRI PRD_PC PT PVEM), then merge with all_months_years,
#    keep if (month==10 & year==2004), rename listanominal, compute turnout, set year=2004, month="November"
################################################################################

df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRD_PC","PT","PVEM")), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    ed      = 28,
    seccion = section
  )

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

df_merged <- df_collapsed %>%
  left_join(df_all, by=c("ed","seccion")) %>%
  filter(month==10, year==2004) %>%
  filter(!is.na(lista))

df_2004<- df_merged %>%
  select(-ed, -seccion, -month, -year) %>%
  rename(listanominal=lista) %>%
  mutate(
    turnout = total / listanominal,
    year    = 2004,
    month   = "November"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Ayu_Seccion_2007.csv" CSV file, rename columns, filter, parse numeric
################################################################################

df_2007 <- read_csv("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2007.csv", 
                    show_col_types=FALSE)
colnames(df_2007) <- tolower(colnames(df_2007))
names(df_2007) <- gsub("[. ]", "", names(df_2007))


df_2007 <-df_2007 %>%
  rename(
    municipality = nombre_municipio,
    section      = seccion,
    listanominal = lista_nominal
  ) %>%
  filter(!(municipality=="" & is.na(section))) %>%  # drop if municipality=="" & section==.
  filter(!(is.na(total) | total==0))               # drop if total==. | total==0

# Convert columns from pan through total to numeric
# In Stata, 'destring listanominal pan - total, replace'
vote_cols <- c("listanominal","pan","pri","prd","pt","pvem","pc","prdpt","pas","total")
df_2007 <- df_2007 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) Collapse (sum) listanominal pan - total, by (municipality, section)
################################################################################

df_collapsed <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(
    across(all_of(intersect(vote_cols, names(.))), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) Rename columns
################################################################################

df_collapsed <- df_collapsed %>%
  rename(
    PAN    = pan,
    PRI    = pri,
    PRD    = prd,
    PT     = pt,
    PVEM   = pvem,
    PC     = pc,
    PRD_PT = prdpt,
    PAS    = pas
  )

################################################################################
# 4) Compute turnout= total/listanominal
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(turnout = total / listanominal)

################################################################################
# 5) Create uniqueid=0 and assign municipality codes
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="ABASOLO"          ~ 28001,
      municipality=="ALDAMA"           ~ 28002,
      municipality=="ALTAMIRA"         ~ 28003,
      municipality=="ANTIGUO MORELOS"  ~ 28004,
      municipality=="BURGOS"           ~ 28005,
      municipality=="BUSTAMANTE"       ~ 28006,
      municipality=="CAMARGO"          ~ 28007,
      municipality=="CASAS"            ~ 28008,
      municipality=="CIUDAD MADERO"    ~ 28009,
      municipality=="CRUILLAS"         ~ 28010,
      municipality=="EL MANTE"         ~ 28021,
      municipality=="GOMEZ FARIAS"     ~ 28011,
      municipality=="GONZALEZ"         ~ 28012,
      municipality=="GUEMEZ"           ~ 28013,
      municipality=="GUERRERO"         ~ 28014,
      municipality=="GUSTAVO DIAZ ORDAZ" ~ 28015,
      municipality=="HIDALGO"          ~ 28016,
      municipality=="JAUMAVE"          ~ 28017,
      municipality=="JIMENEZ"          ~ 28018,
      municipality=="LLERA"            ~ 28019,
      municipality=="MAINERO"          ~ 28020,
      municipality=="MATAMOROS"        ~ 28022,
      municipality=="MENDEZ"           ~ 28023,
      municipality=="MIER"             ~ 28024,
      municipality=="MIGUEL ALEMAN"    ~ 28025,
      municipality=="MIQUIHUANA"       ~ 28026,
      municipality=="NUEVO LAREDO"     ~ 28027,
      municipality=="NUEVO MORELOS"    ~ 28028,
      municipality=="OCAMPO"           ~ 28029,
      municipality=="PADILLA"          ~ 28030,
      municipality=="PALMILLAS"        ~ 28031,
      municipality=="REYNOSA"          ~ 28032,
      municipality=="RIO BRAVO"        ~ 28033,
      municipality=="SAN CARLOS"       ~ 28034,
      municipality=="SAN FERNANDO"     ~ 28035,
      municipality=="SAN NICOLAS"      ~ 28036,
      municipality=="SOTO LA MARINA"   ~ 28037,
      municipality=="TAMPICO"          ~ 28038,
      municipality=="TULA"             ~ 28039,
      municipality=="VALLE HERMOSO"    ~ 28040,
      municipality=="VICTORIA"         ~ 28041,
      municipality=="VILLAGRAN"        ~ 28042,
      municipality=="XICOTENCATL"      ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 6) Compute valid= rowtotal(PAN PRI PRD PRD_PT PT PVEM PC PAS),
#    set year=2007, month="November", sort section
################################################################################

df_2007 <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRD","PRD_PT","PT","PVEM","PC","PAS")), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2007,
    month = "November"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Ayu_Seccion_2010.xlsx", rename columns, drop empties, parse numeric
################################################################################

df_2010 <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2010.xlsx",
  sheet = 1,         # or specify the correct sheet if not the first
  col_names= TRUE
) %>%
  as.data.frame()
colnames(df_2010) <- tolower(colnames(df_2010))
names(df_2010) <- gsub("[- ]", "", names(df_2010))
# rename municipio->municipality, seccion->section (some versions might use "sección")
if ("municipio" %in% names(df_2010)) {
  df_2010 <- df_2010 %>% rename(municipality = municipio)
}
if ("sección" %in% names(df_2010)) {
  df_2010 <- df_2010 %>% rename(section = sección)
}

# drop if municipality=="" & section==.
df_2010 <- df_2010 %>%
  filter(!(municipality=="" & is.na(section)))

# drop if total==. | total==0
df_2010 <- df_2010 %>%
  filter(!is.na(total) & total!=0)

# destring listanominal pan - total => parse numeric
vote_cols <- c("listanominal","pan","pri","pripanal","pripvempanal","prd","pt","pvem","pc","panal",
               "nulos","validos","total")
df_2010 <- df_2010 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) Mark 'missing = listanominal==. or municipality=="Mainero"' => we replicate 
################################################################################

df_2010 <- df_2010 %>%
  mutate(
    missing = if_else(is.na(listanominal) | municipality=="Mainero", 1, 0)
  )

################################################################################
# 3) collapse (sum) missing listanominal pan - total, by (municipality, section)
################################################################################

collapse_cols <- c("missing","listanominal","pan","pri","pripanal","pripvempanal","prd","pt","pvem","pc","panal","nulos","validos","total")
df_collapsed <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(across(all_of(intersect(collapse_cols, names(.))), sum, na.rm=TRUE), .groups="drop")

################################################################################
# 4) Merge with all_months_years for (ed=28, seccion=section, month==6 & year==2010),
#    drop unmatched, replace listanominal if missing>=1 & new lista>old
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(ed=28, seccion=section)

df_all <- read_dta("../../all_months_years.dta") %>%
  select(ed, seccion, month, year, lista)

df_join <- df_collapsed %>%
  left_join(df_all, by=c("ed","seccion")) %>%
  filter(month==6, year==2010) %>%
  filter(!is.na(lista))  # drop if unmatched

df_join <- df_join %>%
  select(-ed, -seccion, -month, -year)

# replace listanominal=lista if missing>=1 & lista>listanominal
df_join <- df_join %>%
  mutate(
    listanominal = if_else(missing>=1 & lista>listanominal, lista, listanominal)
  ) %>%
  select(-lista, -missing)

################################################################################
# 5) rename columns (pan->PAN, pri->PRI, pripanal->PRI_PANAL, etc.), compute turnout
################################################################################

df_join <- df_join %>%
  rename(
    PAN              = pan,
    PRI              = pri,
    PRI_PANAL        = pripanal,
    PRI_PVEM_PANAL   = pripvempanal,
    PRD              = prd,
    PT               = pt,
    PVEM             = pvem,
    PC               = pc,
    PANAL            = panal
  ) %>%
  # drop nulos, validos if they exist
  select(-any_of(c("nulos","validos"))) %>%
  mutate(
    turnout = total / listanominal
  )

################################################################################
# 6) Create uniqueid=0, then assign codes for each municipality
################################################################################

df_join <- df_join %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality=="Abasolo"        ~ 28001,
      municipality=="Aldama"         ~ 28002,
      municipality=="Altamira"       ~ 28003,
      municipality=="Ant. Morelos"   ~ 28004,
      municipality=="Burgos"         ~ 28005,
      municipality=="Bustamante"     ~ 28006,
      municipality=="Camargo"        ~ 28007,
      municipality=="Casas"          ~ 28008,
      municipality=="Cd.Madero"      ~ 28009,
      municipality=="Cruillas"       ~ 28010,
      municipality=="Mante, El"      ~ 28021,
      municipality=="Gomez Farias"   ~ 28011,
      municipality=="González"       ~ 28012,
      municipality=="Guemez"         ~ 28013,
      municipality=="Guerrero"       ~ 28014,
      municipality=="G. Díaz Ordaz"  ~ 28015,
      municipality=="Hidalgo"        ~ 28016,
      municipality=="Jaumave"        ~ 28017,
      municipality=="Jimenez"        ~ 28018,
      municipality=="Llera"          ~ 28019,
      municipality=="Mainero"        ~ 28020,
      municipality=="Matamoros"      ~ 28022,
      municipality=="Méndez"         ~ 28023,
      municipality=="Mier"           ~ 28024,
      municipality=="Miguel Aleman"  ~ 28025,
      municipality=="Miquihuana"     ~ 28026,
      municipality=="Nvo. Laredo"    ~ 28027,
      municipality=="Nvo Morelos"    ~ 28028,
      municipality=="Ocampo"         ~ 28029,
      municipality=="Padilla"        ~ 28030,
      municipality=="Palmillas"      ~ 28031,
      municipality=="Reynosa"        ~ 28032,
      municipality=="Río Bravo"      ~ 28033,
      municipality=="San Carlos"     ~ 28034,
      municipality=="San Fernando"   ~ 28035,
      municipality=="San Nicolás"    ~ 28036,
      municipality=="Soto la Marina" ~ 28037,
      municipality=="Tampico"        ~ 28038,
      municipality=="Tula"           ~ 28039,
      municipality=="Valle Hermoso"  ~ 28040,
      municipality=="Victoria"       ~ 28041,
      municipality=="Villagrán"      ~ 28042,
      municipality=="Xicotencatl"    ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 7) Compute valid= rowtotal(PAN PRI PRI_PANAL PRI_PVEM_PANAL PRD PT PVEM PANAL PC),
#    set year=2010, month="July", sort section, save "Tamaulipas_Section_2010.dta"
################################################################################

df_2010 <- df_join %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(
      "PAN","PRI","PRI_PANAL","PRI_PVEM_PANAL","PRD","PT","PVEM","PANAL","PC"
    )), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2010,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Ayu_Seccion_2013.xlsx", rename columns, drop incomplete or zero
################################################################################

df_2013 <- read_excel(
  path = "../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayu_Seccion_2013.xlsx",
  sheet= 1,      # or specify the correct sheet if not the first
  col_names= TRUE
) %>%
  as.data.frame()
colnames(df_2013) <- tolower(colnames(df_2013))
names(df_2013) <- gsub("[. ]", "", names(df_2013))
# rename municipio->municipality, seccion->section, listadonominal->listanominal
df_2013 <- df_2013 %>%
  rename(
    municipality = municipio,
    section      = seccion,
    listanominal = listadonominal
  )

# drop if municipality=="" or section==.
df_2013 <- df_2013 %>%
  filter(!(municipality=="" | is.na(section)))

# drop if total==. or total==0
df_2013 <- df_2013 %>%
  filter(!is.na(total) & total!=0)

# parse numeric columns: 'listanominal pan - total'
vote_cols <- c("listanominal","pan","pri","pri_pvem_panal","prd","pt","pvem","pc","panal","nulos","noregistrado","total")
df_2013 <- df_2013 %>%
  mutate(across(all_of(intersect(vote_cols, names(.))), as.numeric))

################################################################################
# 2) collapse (sum) listanominal pan - total by (municipality, section)
################################################################################

df_collapse <- df_2013 %>%
  group_by(municipality, section) %>%
  summarise(
    across(all_of(intersect(vote_cols, names(.))), sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) rename columns, compute turnout= total/listanominal, drop nulos, noregistrado
################################################################################

df_collapse <- df_collapse %>%
  rename(
    PAN            = pan,
    PRI            = pri,
    PRI_PVEM_PANAL = pri_pvem_panal,
    PRD            = prd,
    PT             = pt,
    PVEM           = pvem,
    PC             = pc,
    PANAL          = panal
  ) %>%
  mutate(
    turnout = total / listanominal
  ) %>%
  select(-any_of(c("nulos","noregistrado")))

################################################################################
# 4) trim municipality, assign uniqueid=0, then set codes for each municipality
################################################################################

df_collapse <- df_collapse %>%
  mutate(
    municipality = str_trim(municipality),
    uniqueid = 0
  ) %>%
  mutate(
    uniqueid = case_when(
      municipality=="Abasolo"          ~ 28001,
      municipality=="Aldama"           ~ 28002,
      municipality=="Altamira"         ~ 28003,
      municipality=="Antiguo Morelos"  ~ 28004,
      municipality=="Burgos"           ~ 28005,
      municipality=="Bustamante"       ~ 28006,
      municipality=="Camargo"          ~ 28007,
      municipality=="Casas"            ~ 28008,
      municipality=="Ciudad Madero"    ~ 28009,
      municipality=="Cruillas"         ~ 28010,
      municipality=="El Mante"         ~ 28021,
      municipality=="Gómez Farías"     ~ 28011,
      municipality=="González"         ~ 28012,
      municipality=="Güémez"           ~ 28013,
      municipality=="Guerrero"         ~ 28014,
      municipality=="Gustavo Díaz Ordaz" ~ 28015,
      municipality=="Hidalgo"          ~ 28016,
      municipality=="Jaumave"          ~ 28017,
      municipality=="Jiménez"          ~ 28018,
      municipality=="Llera"            ~ 28019,
      municipality=="Mainero"          ~ 28020,
      municipality=="Matamoros"        ~ 28022,
      municipality=="Méndez"           ~ 28023,
      municipality=="Mier"             ~ 28024,
      municipality=="Miguel Alemán"    ~ 28025,
      municipality=="Miquihuana"       ~ 28026,
      municipality=="Nuevo Laredo"     ~ 28027,
      municipality=="Nuevo Morelos"    ~ 28028,
      municipality=="Ocampo"           ~ 28029,
      municipality=="Padilla"          ~ 28030,
      municipality=="Palmillas"        ~ 28031,
      municipality=="Reynosa"          ~ 28032,
      municipality=="Río Bravo"        ~ 28033,
      municipality=="San Carlos"       ~ 28034,
      municipality=="San Fernando"     ~ 28035,
      municipality=="San Nicolás"      ~ 28036,
      municipality=="Soto la Marina"   ~ 28037,
      municipality=="Tampico"          ~ 28038,
      municipality=="Tula"             ~ 28039,
      municipality=="Valle Hermoso"    ~ 28040,
      municipality=="Victoria"         ~ 28041,
      municipality=="Villagrán"        ~ 28042,
      municipality=="Xicoténcatl"      ~ 28043,
      TRUE ~ uniqueid
    )
  )

################################################################################
# 5) compute valid= rowtotal(PAN PRI PRI_PVEM_PANAL PRD PT PVEM PANAL PC),
#    year=2013, month="July", sort, save
################################################################################

df_2013 <- df_collapse %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c("PAN","PRI","PRI_PVEM_PANAL","PRD","PT","PVEM","PANAL","PC")), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2013,
    month = "July"
  ) %>%
  arrange(section)


################################################################################
# Part A: Reading each sheet from "Ayuntamientos_2016.xlsx" into .dta
################################################################################

all_sheets <- excel_sheets("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayuntamientos_2016.xlsx")

for (sheetname in all_sheets) {
  # Read the Excel sheet
  df_sheet <- read_excel(
    path = "../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Ayuntamientos_2016.xlsx",
    sheet = sheetname,
    col_names = TRUE,
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # Clean column names to make them valid for Stata
  colnames(df_sheet) <- colnames(df_sheet) %>%
    gsub("[^a-zA-Z0-9_]", "_", .) %>%  # Replace invalid characters with underscores
    gsub("_+", "_", .) %>%            # Replace multiple underscores with a single one
    gsub("^_|_$", "", .) %>%          # Remove leading or trailing underscores
    substr(1, 32)                     # Truncate to 32 characters
  
  # Ensure uniqueness of truncated column names
  colnames(df_sheet) <- make.unique(colnames(df_sheet))
  
  # Add municipio column and filter rows where Casilla is not empty
  if ("Casilla" %in% names(df_sheet)) {
    df_sheet <- df_sheet %>%
      mutate(municipio = sheetname) %>%
      filter(Casilla != "")
  }
  
  # Replace empty strings with "0" for all columns
  df_sheet <- df_sheet %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # Save as a .dta file
  write_dta(df_sheet, paste0(sheetname, ".dta"))
}


################################################################################
# Part B: Appending all "Table *.dta" files named after each municipality
################################################################################

append_files <- c(
  "ABASOLO.dta","ALDAMA.dta","ALTAMIRA.dta","ANTIGUO MORELOS.dta","BURGOS.dta","BUSTAMANTE.dta",
  "CAMARGO.dta","CASAS.dta","CIUDAD MADERO.dta","CRUILLAS.dta","GOMEZ FARIAS.dta","GONZALEZ.dta",
  "GUEMEZ.dta","GUERRERO.dta","GUSTAVO DIAZ ORDAZ.dta","HIDALGO.dta","JAUMAVE.dta","JIMENEZ.dta",
  "LLERA.dta","MAINERO.dta","EL MANTE.dta","MATAMOROS.dta","MENDEZ.dta","MIER.dta","MIGUEL ALEMAN.dta",
  "MIQUIHUANA.dta","NUEVO LAREDO.dta","NUEVO MORELOS.dta","OCAMPO.dta","PADILLA.dta","PALMILLAS.dta",
  "REYNOSA.dta","RIO BRAVO.dta","SAN CARLOS.dta","SAN FERNANDO.dta","SAN NICOLAS.dta",
  "SOTO LA MARINA.dta","TAMPICO.dta","TULA.dta","VALLE HERMOSO.dta","VICTORIA.dta",
  "VILLAGRAN.dta","XICOTENCATL.dta"
)

df_all <- data.frame()

for (f in append_files) {
  if (file.exists(f)) {
    df_new <- read_dta(f)
    df_all <- bind_rows(df_all, df_new)
  }
}

# Now we can erase these .dta files if needed:
for (f in append_files) {
  if (file.exists(f)) {
    file.remove(f)
  }
}

################################################################################
# Part C: Data Cleaning (destring, keep certain columns, drop U, create CI_1..CI_3)
################################################################################
names(df_all) <- gsub("[_]", "", names(df_all))
df_all <- df_all %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))


keep_cols <- c("Casilla","PAN","PRI","PRD","MC","PANAL","MORENA","PES","CoaliciónPRIPVEMyPANAL",
               "PRIPVEM","PRIPNA","PVEMPNA","CandidatosNoRegistrados","VotosNulos",
               "JOSEADAME","PABLOTORRESLARA","XICOTENCATLGONZALEZURESTI", "AMANDOTREVIÑORIVERA",
               # plus the columns for the "egen CI_1..CI_3" logic
               "JUANCUAUHTEMOCGARCIATAMEZ","JOSERAMONGOMEZLEAL","CARLOSRAFAELULIVARRILOPEZ",
               "AMANDOTREVIÑORIVERA","HECTORPEÑASALDAÑA","CI_1","CI_2","CI_3" # if they exist
               # adapt as needed
)

df_all <- df_all %>% select(any_of(keep_cols))

# drop U if it exists
df_all <- df_all %>%
  select(-any_of("U"))

# egen CI_1=rowtotal(JUANCUAUHTEMOCGARCIATAMEZ-JOSERAMONGOMEZLEAL CARLOSRAFAELULIVARRILOPEZ-XICOTENCATLGONZALEZURESTI)
# This is tricky. In Stata, the syntax "JUANCUAUHTEMOCGARCIATAMEZ-JOSERAMONGOMEZLEAL" is a range of columns. 
# We'll interpret you want to sum these columns: (JUANCUAUHTEMOCGARCIATAMEZ, ..., JOSERAMONGOMEZLEAL, CARLOSRAFAELULIVARRILOPEZ, ..., XICOTENCATLGONZALEZURESTI).
# Then you do second row for "egen CI_2", etc. We'll replicate the sums explicitly.

df_all <- df_all %>%
  rowwise() %>%
  mutate(
    CI_1 = sum(
      c_across(c("JUANCUAUHTEMOCGARCIATAMEZ","JOSERAMONGOMEZLEAL","CARLOSRAFAELULIVARRILOPEZ")),
      na.rm=TRUE
    ),
    CI_2 = sum(
      c_across(c("AMANDOTREVIÑORIVERA","HECTORPEÑASALDAÑA","JOSERAMONGOMEZLEAL","PABLOTORRESLARA")),
      na.rm=TRUE
    ),
    CI_3 = sum(
      c_across(c("JESUSROBERTOGUERRAVELASCO","EDUARDOLONGORIACHAPA","CARLOSRAFAELULIVARRILOPEZ")), # adapt if exist
      na.rm=TRUE
    )
  ) %>%
  ungroup()

# replace CI_1=CI_1 - CI_2 - CI_3
df_all <- df_all %>%
  mutate(
    CI_1 = CI_1 - CI_2 - CI_3
  )

# drop columns used in row sum if needed
df_all <- df_all %>%
  select(-any_of(c("JUANCUAUHTEMOCGARCIATAMEZ","JOSERAMONGOMEZLEAL","CARLOSRAFAELULIVARRILOPEZ","AMANDOTREVIÑORIVERA","HECTORPEÑASALDAÑA","JESUSROBERTOGUERRAVELASCO","EDUARDOLONGORIACHAPA")))

################################################################################
# Part D: Generating section from substring of Casilla, removing letters, etc.
################################################################################

df_all <- df_all %>%
  mutate(
    section = substr(Casilla, 1, 4)
  ) %>%
  mutate(
    section = if_else(str_detect(section,"B|C|E"), substr(section,1,3), section)
  )

# destring section, replace
df_all <- df_all %>%
  mutate(section = suppressWarnings(as.numeric(section))) %>%
  filter(!is.na(section))

################################################################################
# Part E: Rename columns, do coalition merges, drop duplicates, collapse (sum)
################################################################################

df_all <- df_all %>%
  rename(
    municipality="municipio",
    PANAL="PNA",  # from code or adapt
    MORENA="Morena",
    PES="ES",
    PRI_PVEM_PANAL="CoaliciónPRIPVEMyPANAL",
    PRI_PVEM="PRIPVEM",
    PRI_PANAL="PRIPNA",
    PVEM_PANAL="PVEMPNA",
    no_reg="CandidatosNoRegistrados",
    nulo="VotosNulos"
  )

# Then do the merges for coalitions
df_all <- df_all %>%
  mutate(
    PRI_PVEM_PANAL = if_else(!is.na(PRI_PVEM_PANAL),
                             PRI_PVEM_PANAL + coalesce(PRIPVEM,0)+coalesce(PRIPNA,0)+coalesce(PVEMPNA,0)+coalesce(PRI,0)+coalesce(PVEM,0)+coalesce(PNA,0),
                             PRI_PVEM_PANAL
    ),
    PRI   = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PRI),
    PVEM  = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PVEM),
    PNA   = if_else(!is.na(PRI_PVEM_PANAL), NA_real_, PNA)
  ) %>%
  select(-any_of(c("PRIPVEM","PRIPNA","PVEMPNA")))

# order CI_* after "PRI_" => adapt if you want a custom reorder
# collapse (sum) PAN-nulo, by(municipality section)
vote_cols2 <- c("PAN","PRI","PRD","PVEM","PT","MC","PNA","Morena","ES","PRI_PVEM_PANAL","CI_1","CI_2","CI_3","nulo","no_reg")

df_collapse2 <- df_all %>%
  group_by(municipality, section) %>%
  summarise(across(any_of(vote_cols2), sum, na.rm=TRUE), .groups="drop")

################################################################################
# Part F: compute valid, total, drop no_reg nulo, merge municipality codes, merge LN2016, final
################################################################################

# valid=rowtotal(PAN PRI PRD PVEM PT MC PNA Morena ES PRI_PVEM_PANAL CI_1..CI_3)
df_collapse2 <- df_collapse2 %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(c("PAN","PRI","PRD","PVEM","PT","MC","PNA","Morena","ES","PRI_PVEM_PANAL","CI_1","CI_2","CI_3"))), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    total = valid + coalesce(nulo,0) + coalesce(no_reg,0)
  ) %>%
  select(-nulo, -no_reg)

# 1) Merge municipality codes from "uniqueid16.xlsx"
df_ids <- read_excel("uniqueid16.xlsx") %>%
  as.data.frame()

df_merge_ids <- df_collapse2 %>%
  left_join(df_ids, by="municipality") %>%
  select(-starts_with("_merge"))

# 2) Merge LN2016 => from code: "LN2016.dta"
df_ln16 <- read_dta("../Listas Nominales/LN 2012-2019/2016/LN2016.dta") %>%
  filter(entidad==28, month==5) %>%
  rename(section=seccion, listanominal=lista) %>%
  select(section, listanominal)

df_final <- df_merge_ids %>%
  left_join(df_ln16, by="section") %>%
  filter(!is.na(listanominal))

# turnout = total / listanominal, year=2016, month="June", STATE="TAMAULIPAS"
df_2016 <- df_final %>%
  mutate(
    turnout = total / listanominal,
    year    = 2016,
    month   = "June",
    STATE   = "TAMAULIPAS"
  )

################################################################################
# Part A: Reading each Excel (like "Abasolo.xlsx", "Aldama.xlsx"...) from 
#         "Ayuntamientos 2018" folder, cleaning, converting, then saving as .dta
################################################################################

# We'll define a vector of municipality sheet/file names
mun_names <- c("Abasolo","Aldama","Altamira","AntiguoMorelos","Burgos","Bustamente",
               "Camargo","Casas","CiudadMadero","Cruillas","ElMante","GomezFarias",
               "Gonzalez","Guemez","Guerrero","GustavoDiazOrdaz","Hidalgo","Jaumave",
               "Jimenez","Llera","Mainero","Matamoros","Mendez","Mier","MiguelAleman",
               "Miquihuana","NuevoLaredo","NuevoMorelos","Ocampo","Padilla","Palmillas",
               "Reynosa","RioBravo","SanCarlos","SanFernando","SanNicolas","SotolaMarina",
               "Tampico","Tula","ValleHermoso","Victoria","Villagran","Xicotencatl")

# We'll iterate over these municipality names, read them, do the clean steps, save as .dta
for (x in mun_names) {
  # "import excel `x'.xlsx, clear"
  df_mun <- read_excel(
    path = paste0("../../../Data/Raw Electoral Data/Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013,2016,2018/Other/Ayuntamientos 2018/", x, ".xlsx"),
    sheet=1,  # if only 1 sheet, or specify the correct sheet
    col_names=TRUE,
    col_types="text"
  ) %>%
    as.data.frame()
  
  # drop if _n<=5 => in R, we can do row_number
  # The code "drop if _n<=5" in Stata means drop first 5 rows. We'll replicate:
  df_mun <- df_mun %>%
    slice(-(1:5))  # remove first 5 rows
  
  # export excel using "updated_`x'.xlsx", replace => skip that step in R, or do it if needed
  # We won't do an Excel re-export. We'll replicate the final steps:
  
  # import excel "updated_`x'.xlsx", firstrow clear => we skip that too
  
  # Convert all columns to string, then drop rows if "SECCION" is ""
  df_mun <- df_mun %>%
    mutate(across(everything(), as.character)) %>%
    filter(SECCION != "")
  
  # For each column: replace "" with "0"
  df_mun <- df_mun %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # "save `x'.dta, replace"
  # We'll write the result as `<x>.dta`
  write_dta(df_mun, paste0(x, ".dta"))
  
  # "erase updated_`x'.xlsx" => we skip removing that Excel unless we created it
}

################################################################################
# Part B: Appending all municipality .dta files
################################################################################

df_all <- data.frame()
for (x in mun_names) {
  # append using "<x>.dta"
  fpath <- paste0(x, ".dta")
  if (file.exists(fpath)) {
    df_new <- read_dta(fpath)
    df_all <- bind_rows(df_all, df_new)
    
    # erase "<x>.dta"
    file.remove(fpath)
  }
}

################################################################################
# Part C: Data cleaning steps (like "destring, keep certain columns, rename, etc.")
################################################################################

# In Stata:
# destring *, replace => parse numeric columns if they exist
df_all <- df_all %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))

# keep Casilla-PABLOTORRESLARA JOSEADAME XICOTENCATLGONZALEZURESTI => 
# This code suggests you keep columns from "Casilla" to "XICOTENCATLGONZALEZURESTI" plus some named columns in between
# We'll define some subset if needed. 
# The code references columns up to "XICOTENCATLGONZALEZURESTI", plus "JOSEADAME" 
# So adapt to your actual column set:

keep_cols <- c("Casilla","PAN","PRI","PRD","MC","PANAL","MORENA","PES",
               "PTMORENAES","PTMORENA","PTES","MORENAES","PANPRDMC","PANPRD","PANMC","PRDMC",
               "VotosNulos","CandidatosNoRegistrados","HECTORPEÑASALDAÑA","JOSEADAME","PABLOTORRESLARA",
               "XICOTENCATLGONZALEZURESTI","NA","ES","PT","PTMC" # adapt as needed
               # plus other columns if you have them
)
df_all <- df_all %>%
  select(any_of(keep_cols))

# drop U if present
df_all <- df_all %>% select(-any_of("U"))

# Create CI_1, CI_2, CI_3 from row sums
# e.g. 
# egen CI_1=rowtotal(JESUSOLVERAMENDEZ ... XICOTENCATLGONZALEZURESTI)
# We'll do it explicitly if we know the columns that go in:

df_all <- df_all %>%
  rowwise() %>%
  mutate(
    CI_1 = sum(c_across(any_of(c("JESUSOLVERAMENDEZ","DAVIDPERALESSEGURA",
                                 "BEATRIZREYESNAJERA","JOSELUISGALLARDOFLORES",
                                 "HECTORMANUELDELATORREVALENZ","HUMBERTORANGELVALLEJO",
                                 "VICTORMANUELVERGARAMARTINEZ","JORGELUISMIRANDANIÑO",
                                 "HECTORMICHELSALINASGAMEZ","CARLOSALBERTOGUERREROGARCIA",
                                 "MIGUELANGELALMARAZMALDONADO","CLAUDIOALBERTOCAPETILLOGOMEZ",
                                 "NAYMAKARINABALQUIARENAPEREZ","HECTORDAVIDRUIZTAMAYO",
                                 "XICOTENCATLGONZALEZURESTI"
    ))), na.rm=TRUE),
    CI_2 = sum(c_across(any_of(c("AMANDOTREVIÑORIVERA","HECTORPEÑASALDAÑA",
                                 "JOSERAMONGOMEZLEAL","PABLOTORRESLARA"))), na.rm=TRUE),
    CI_3 = sum(c_across(any_of(c("JESUSROBERTOGUERRAVELASCO","EDUARDOLONGORIACHAPA",
                                 "CARLOSRAFAELULIVARRILOPEZ"))), na.rm=TRUE)
  ) %>%
  ungroup()

# replace CI_1=CI_1 - CI_2 - CI_3
df_all <- df_all %>%
  mutate(CI_1 = CI_1 - CI_2 - CI_3)

# drop columns used in CI_1 sums if needed
rm_cols <- c("JESUSOLVERAMENDEZ","DAVIDPERALESSEGURA","BEATRIZREYESNAJERA","JOSELUISGALLARDOFLORES",
             "HECTORMANUELDELATORREVALENZ","HUMBERTORANGELVALLEJO","VICTORMANUELVERGARAMARTINEZ",
             "JORGELUISMIRANDANIÑO","HECTORMICHELSALINASGAMEZ","CARLOSALBERTOGUERREROGARCIA",
             "MIGUELANGELALMARAZMALDONADO","CLAUDIOALBERTOCAPETILLOGOMEZ","NAYMAKARINABALQUIARENAPEREZ",
             "HECTORDAVIDRUIZTAMAYO","AMANDOTREVIÑORIVERA","JOSERAMONGOMEZLEAL","PABLOTORRESLARA",
             "JESUSROBERTOGUERRAVELASCO","EDUARDOLONGORIACHAPA","CARLOSRAFAELULIVARRILOPEZ"
)
df_all <- df_all %>% select(-any_of(rm_cols))

################################################################################
# Convert Casilla => numeric section (like substring)
################################################################################

df_all <- df_all %>%
  mutate(
    section = str_sub(Casilla, 1, 4),
    section = if_else(str_detect(section,"B|C|E"), str_sub(section,1,3), section)
  ) %>%
  mutate(section = suppressWarnings(as.numeric(section))) %>%
  filter(!is.na(section))

################################################################################
# Rename columns from code
################################################################################

df_all <- df_all %>%
  rename(
    municipality = MUNICIPIO,
    PANAL        = NA,
    MORENA       = Morena,
    PES          = ES
    # etc. adapt to your actual columns
  )

# Some coalition merges: 
# e.g. g MORENA_PT_PES= MORENA + PT + ES + PTMORENAES + ...
# We'll do explicitly if you have them:

df_all <- df_all %>%
  mutate(
    MORENA_PT_PES = if_else(municipality!="BURGOS" & municipality!="NUVEO MORELOS",
                            coalesce(MORENA,0) + coalesce(PT,0) + coalesce(PES,0) + coalesce(PTMORENAES,0) + coalesce(PTMORENA,0) + coalesce(PTES,0) + coalesce(MORENAES,0),
                            NA_real_
    ),
    # set those columns to NA if municipality != ...
  ) %>%
  # etc. for PAN_PRD_MC 
  mutate(
    PAN_PRD_MC = if_else(
      municipality %in% c("BURGOS","BUSTAMANTE","CASAS","CRUILLAS","EL MANTE","GOMEZ FARIAS","GONZALEZ","GUERRERO",
                          "JAUMAVE","JIMENEZ","MENDEZ","MIER","MIQUIHUANA","NUVEO MORELOS","OCAMPO","PADILLA","PALMILLAS",
                          "SAN NICOLAS","TULA","VILLAGRAN"),
      NA_real_,
      coalesce(PAN,0)+coalesce(PRD,0)+coalesce(MC,0)+coalesce(PANPRDMC,0)+coalesce(PANPRD,0)+coalesce(PANMC,0)+coalesce(PRDMC,0)
    )
  )

# Then we set them to NA or 0 as needed.

################################################################################
# Final: collapse (sum) PAN-nulo, rename columns, merge with uniqueid16.dta, etc.
################################################################################

vote_cols3 <- c("PAN","PRI","PRD","PVEM","PT","MC","PANAL","MORENA","PES","MORENA_PT_PES","PAN_PRD_MC",
                "CI_1","CI_2","CI_3","...","valid","total","listanominal")

df_collapse3 <- df_all %>%
  group_by(municipality, section) %>%
  summarise(across(any_of(vote_cols3), sum, na.rm=TRUE), .groups="drop")

# replace municipality="GUEMEZ" if municipality=="GÜEMEZ"
df_collapse3 <- df_collapse3 %>%
  mutate(municipality = if_else(municipality=="GÜEMEZ","GUEMEZ", municipality))

# merge m:1 municipality using uniqueid16.dta
df_ids <- read_dta("uniqueid16.dta")
df_merge_ids <- df_collapse3 %>%
  left_join(df_ids, by="municipality") %>%
  select(-starts_with("_merge"))

# set g year=2018, month="July", STATE="TAMAULIPAS"
df_2018 <- df_merge_ids %>%
  mutate(
    year  = 2018,
    month = "July",
    STATE = "TAMAULIPAS"
  )

# Combine the dataframes, handling different columns by filling with NA
Tamaulipas_all <- bind_rows(df_1995,
                         df_1998,
                         df_2001,
                         df_2004,
                         df_2007,
                         df_2010,
                         df_2013,
                         df_2016,
                         df_2018)

# Replace municipality = upper(municipality), then unify some names
Tamaulipas_all <- Tamaulipas_all %>%
  mutate(
    municipality = str_to_upper(municipality),
    municipality = if_else(municipality=="PALMILLA","PALMILLAS", municipality),
    municipality = if_else(municipality=="MIGUEL A.","MIGUEL ALEMAN", municipality),
    municipality = if_else(municipality %in% c("ANT. MORELOS","A. MORELOS"),"ANTIGUO MORELOS", municipality),
    municipality = if_else(municipality %in% c("CD. MADERO","CD.MADERO"),"CIUDAD MADERO", municipality),
    municipality = if_else(str_detect(municipality,"ORDAZ") | municipality=="GVO. DIAZ O.","GUSTAVO DIAZ ORDAZ", municipality),
    municipality = if_else(str_detect(municipality,"MANTE") & municipality!="BUSTAMANTE","EL MANTE", municipality),
    municipality = if_else(str_detect(municipality,"MORELOS") & municipality!="ANTIGUO MORELOS","NUEVO MORELOS", municipality),
    municipality = if_else(str_detect(municipality,"LAREDO"),"NUEVO LAREDO", municipality)
  )

data.table::fwrite(Tamaulipas_all,"../../../Processed Data/tamaulipas/Tamaulipas_process_raw_data.csv")

