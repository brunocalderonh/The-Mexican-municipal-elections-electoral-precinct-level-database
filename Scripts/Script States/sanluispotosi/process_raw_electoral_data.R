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
df_1997 <- read_csv("../../../Data/Raw Electoral Data/Morelos - 1997, 2000, 2003, 2006, 2009, 2012,2015,2018/Ayu_Seccion_1997_No_LN.csv") 
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

# uniqueid replacements
df_1997 <- df_1997 %>%
  mutate(
    uniqueid = case_when(
      municipality == "AHUALULCO" ~ 24001,
      municipality == "ALAQUINES" ~ 24002,
      municipality == "AQUISMON"  ~ 24003,
      # ... etc. (apply all replacements) ...
      municipality == "ZARAGOZA"  ~ 24055,
      TRUE ~ 0
    )
  )

# Lines merging with all_months_years.dta
df_all <- read_dta("../../all_months_years.dta") %>%
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

# aggregator lines => omitted

# rowranks => omitted
# winner => omitted

df_1997 <- df_1997 %>%
  mutate(
    year  = 1997,
    month = "July"
  )

# Next line: second round for some municipalities => we replicate that logic
df_1997 <- df_1997 %>%
  mutate(
    month = ifelse(
      municipality %in% c("ALAQUINES","AXTLA DE TERRAZAS","CARDENAS","CATORCE",
                          "CEDRAL","CIUDAD DEL MAIZ","CIUDAD VALLES","CHARCAS",
                          "MATEHUALA","RAYON","RIOVERDE","SAN MARTIN CHALCHICUAUTLA",
                          "SAN NICOLAS TOLENTINO","SANTO DOMINGO","TAMASOPO",
                          "TAMPAMOLON CORONA","TAMUIN","TANQUIAN DE ESCOBEDO",
                          "VENEDO","VILLA DE ARISTA","VILLA DE GUADALUPE",
                          "VILLA DE RAMOS","ZARAGOZA"),
      "August", month
    ),
    municipality = ifelse(month == "August", municipality %>% paste0(" RUNOFF"), municipality)
  )

df_1997 <- df_1997 %>%
  mutate(runoff = (month == "August"))

# Save
write_dta(df_1997, "San_Luis_Potosi_Section_1997.dta", version=14)

##############################################################################
# ============== 2) Runoffs 1997.xlsx  =======================================
##############################################################################

df_runoff_1997 <- read_excel("Runoffs 1997.xlsx", sheet = "Sheet1",
                             range = "A4:O1019", col_names = TRUE) %>%
  rename(section = B) %>%
  filter(section != "") %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  mutate(municipality = paste0(DES_PUEBLO," RUNOFF")) %>%
  rename(NPP = NAVA) # rename NAVA->NPP

df_runoff_1997 <- df_runoff_1997 %>%
  mutate(uniqueid = case_when(
    municipality == "AHUALULCO RUNOFF" ~ 24001,
    # ... etc. ...
    municipality == "ZARAGOZA RUNOFF"  ~ 24055,
    TRUE ~ NA_real_
  ))

# Then merging with all_months_years.dta => replicate partial logic
df_all <- read_dta("../../all_months_years.dta") %>%
  filter(ed==24, month==7, year==1997) %>%
  rename(listanominal=lista, section=seccion) %>%
  select(section, listanominal)

# 1:1 merge on section
df_runoff_1997 <- df_runoff_1997 %>%
  left_join(df_all, by="section") %>%
  filter(!is.na(listanominal))

# turnout = total / listanominal
df_runoff_1997 <- df_runoff_1997 %>%
  mutate(turnout = total/listanominal)

# aggregator and rowranks => omitted
# winner => omitted

df_runoff_1997 <- df_runoff_1997 %>%
  mutate(
    year  = 1997,
    month = "August",
    runoff= 1
  )

write_dta(df_runoff_1997, "San_Luis_Potosi_Section_1997_runoff.dta", version=14)

##############################################################################
# ============== 3) Ayu_Seccion_2000.csv  ====================================
##############################################################################
df_2000 <- read_csv("Ayu_Seccion_2000.csv") %>%
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

# uniqueid
df_2000 <- df_2000 %>%
  mutate(uniqueid = case_when(
    municipality == "AHUALULCO" ~ 24001,
    # ... etc. ...
    municipality == "ZARAGOZA"  ~ 24055,
    TRUE ~ 0
  ))

# aggregator => omitted
# rowranks => omitted
# winner => omitted

df_2000 <- df_2000 %>%
  mutate(
    year  = 2000,
    month = "July"
  ) %>%
  mutate(
    month = ifelse(
      municipality %in% c("AQUISMON RUNOFF", "CERRO DE SAN PEDRO RUNOFF", ... # and so on
      ),
      "August",
      month
    ),
    municipality = ifelse(month=="August", paste(municipality, "RUNOFF"), municipality)
  ) %>%
  mutate(runoff = (month=="August"))

write_dta(df_2000, "San_Luis_Potosi_Section_2000.dta", version=14)

##############################################################################
# ============== 4) Ayu_Seccion_2003.csv  ====================================
##############################################################################
df_2003 <- read_csv("Ayu_Seccion_2003.csv") %>%
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
  mutate(uniqueid = case_when(
    municipality == "AHUALULCO" ~ 24001,
    # ...
    municipality == "ZARAGOZA"  ~ 24055,
    TRUE ~ 0
  ))

# aggregator => omitted
# rowranks => omitted
# winner => omitted

df_2003 <- df_2003 %>%
  mutate(
    year  = 2003,
    month = "October",
    runoff= 0
  )

write_dta(df_2003, "San_Luis_Potosi_Section_2003.dta", version=14)

##############################################################################
# ============== 5) Runoffs 2003.xlsx  =======================================
##############################################################################
df_runoff_2003 <- read_excel("Runoffs 2003.xlsx", sheet="Sheet1", range="A6:X902",
                             col_names=TRUE) %>%
  # rename D->section, drop if E=="", rename F->listanominal, rename Votos->total, etc.
  mutate(across(everything(), ~ ifelse(.=="-", "0", .))) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  mutate(municipality = case_when(
    B==21 ~ "MEXQUITIC DE CARMONA RUNOFF",
    # etc...
    TRUE ~ POLITICA
  ))

# aggregator => omitted
# rowranks => omitted
# winner => omitted

df_runoff_2003 <- df_runoff_2003 %>%
  mutate(
    year   = 2003,
    month  = "November",
    runoff = 1
  )

write_dta(df_runoff_2003, "San_Luis_Potosi_Section_2003_runoff.dta", version=14)

##############################################################################
# ============== 6) Ayu_Seccion_2006.csv  ====================================
##############################################################################
df_2006 <- read_csv("Ayu_Seccion_2006.csv") %>%
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
      municipality=="AHUALULCO"~24001,
      # ...
      municipality=="ZARAGOZA" ~24055,
      TRUE~0
    ),
    year  = 2006,
    month = "July"
  ) %>%
  arrange(section)


##############################################################################
# ============== 7) Ayu_Seccion_2009.csv  ====================================
##############################################################################
df_2009 <- read_csv("Ayu_Seccion_2009.csv") %>%
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
      municipality=="AHUALULCO"~24001,
      # ...
      municipality=="ZARAGOZA" ~24055,
      TRUE~0
    ),
    year  = 2009,
    month = "July"
  )



##############################################################################
# ============== 8) Ayu_Seccion_2012.dta  ====================================
##############################################################################
df_2012 <- read_dta("Ayu_Seccion_2012.dta") %>%
  mutate(turnout = total/listanominal) %>%
  mutate(municipality = trimws(municipality)) %>%
  mutate(
    uniqueid = case_when(
      grepl("AHUALULCO", municipality)~24001,
      # ...
      grepl("ZARAGOZA", municipality)~24055,
      TRUE~0
    )
  )


df_2012 <- df_2012 %>%
  filter(section!=1107) %>%
  arrange(section) %>%
  mutate(
    year  = 2012,
    month = "July"
  )


all_slp <- bind_rows(df_1997, df_2000, df_2003, df_2003ro)

# replicate the bysort uniqueid year: egen runoff2=mean(runoff)
all_slp <- all_slp %>%
  group_by(uniqueid, year) %>%
  mutate(runoff2 = mean(runoff, na.rm=TRUE)) %>%
  ungroup()

# drop if runoff==0 & runoff2>0 & year==2003
all_slp <- all_slp %>%
  filter(!(runoff==0 & runoff2>0 & year==2003)) %>%
  select(-runoff2)

# append the rest
all_slp <- bind_rows(all_slp, df_2006, df_2009, df_2012)

##############################################################################
# ============== 10) Ayuntamientos_2015.xlsx  ===============================
##############################################################################
# Import each sheet, drop if Seccion=="", fill empty strings with "0", save .dta
excel_file_2015 <- "Ayuntamientos_2015.xlsx"
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
  "AHUALULCO.dta","ALAQUINES.dta","AQUISMON.dta","ARMADILLO DE LOS INFANTE.dta",
  "VILLA DE ARRIAGA.dta","AXTLA DE TERRAZAS.dta", # ...
  "ZARAGOZA.dta"
  # (Add all listed in the script)
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
  mutate(uniqueid = case_when(
    grepl("AHUALULCO", municipality)~24001,
    # ...
    grepl("ZARAGOZA", municipality)~24055,
    TRUE~0
  ))

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

write_dta(all_2015, "San_Luis_Potosi_Section_2015.dta")

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
excel_file_2018 <- "MunicipiosSLP_2018.xlsx"
sheet_names_2018 <- excel_sheets(excel_file_2018)

for (sname in sheet_names_2018) {
  df_sheet <- read_excel(excel_file_2018, sheet=sname, col_types="text") %>%
    mutate(municipality = sname) %>%
    filter(CASILLAACTA!="") %>%
    mutate(across(everything(), ~ifelse(.=="", "0", .)))
  
  safe_sname <- gsub("[^A-Za-z0-9_\\. ]", "_", sname)
  write_dta(df_sheet, paste0(safe_sname, ".dta"))
}

# Append them
files_2018 <- c(
  "Ahualulco.dta","Alaquines.dta","Aquismon.dta","Armadillo.dta","Cardenas.dta",
  # ...
  "Matlapa.dta"
)

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
  # rename *PMC->*MC, *PNA->PANAL, etc.
  # aggregator => omitted
  # rowranks => omitted
  # winner => omitted
  mutate(
    year  = 2018,
    month = "July",
    STATE = "SAN LUIS POTOSI"
  )

# Merge m:1 municipality using uniqueids.dta => replicate partial
df_uniqueids <- read_dta("uniqueids.dta")  # created by code above
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
df_ln18 <- read_dta("../Listas Nominales/ListadoNominalPREP2018.dta") %>%
  filter(STATE=="SAN LUIS POTOSI") %>%
  rename(listanominal=ListadoNominalINE)

all_2018 <- all_2018 %>%
  left_join(df_ln18, by=c("STATE","SECCION"="section")) %>%
  filter(!is.na(listanominal))

all_2018 <- all_2018 %>%
  mutate(
    turnout = total/listanominal
    # aggregator => omitted
  )


# aggregator => omitted

# Erase the .dta files
for (f in files_2018) {
  if (file.exists(f)) file.remove(f)
}

##############################################################################
# ============== 12) Append 2015 + 2018, then with older data ===============
##############################################################################

all_15_18 <- bind_rows(df_2015, df_2018)


# Then: use "..\..\Precinct\San_Luis_Potosi_ALL.dta", clear
# append using San_Luis_Potosi_Section_15_18.dta
if (file.exists("../../Precinct/San_Luis_Potosi_ALL.dta")) {
  older_slp <- read_dta("../../Precinct/San_Luis_Potosi_ALL.dta")
  all_slp_final <- bind_rows(older_slp, all_15_18)
} else {
  all_slp_final <- all_15_18
}

##############################################################################
# Done
##############################################################################


data.table::fwrite(all_slp_final,"../../../Processed Data/sanluispotosi/slp_process_raw_data.csv")

