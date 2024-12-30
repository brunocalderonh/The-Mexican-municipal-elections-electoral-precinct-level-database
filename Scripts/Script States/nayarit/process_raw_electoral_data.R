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

# ------------------------------------------------------------------------------
# Read 1996 data
df1996 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_1996_No_LN.csv", stringsAsFactors = FALSE)
colnames(df1996) <- tolower(colnames(df1996))

names(df1996)[names(df1996) == "municipio"] <- "municipality"
names(df1996)[names(df1996) == "seccion"]   <- "section"

# drop if municipality=="" & section==.
df1996 <- subset(df1996, !(municipality == "" & is.na(section)))

# create total = rowtotal(pan pri prd pt pvem prs parm cd noregistrados nulos)
df1996$total <- rowSums(df1996[, c("pan","pri","prd","pt","pvem","prs","parm","cd","noregistrados","nulos")],
                        na.rm = TRUE)

# Convert relevant columns to numeric (similar to destring)
num_vars_96 <- c("pan","pri","prd","pt","pvem","prs","parm","cd","noregistrados","nulos","total")
for(v in num_vars_96){
  if(v %in% colnames(df1996)){
    df1996[[v]] <- as.numeric(df1996[[v]])
  }
}

# Omit "collapse (sum) ... by(municipality section)" to keep municipality-section level.

# rename lower-case columns to final uppercase
names(df1996)[names(df1996) == "pan"]  <- "PAN"
names(df1996)[names(df1996) == "pri"]  <- "PRI"
names(df1996)[names(df1996) == "prd"]  <- "PRD"
names(df1996)[names(df1996) == "pt"]   <- "PT"
names(df1996)[names(df1996) == "pvem"] <- "PVEM"
names(df1996)[names(df1996) == "prs"]  <- "PRS"
names(df1996)[names(df1996) == "parm"] <- "PARM"
names(df1996)[names(df1996) == "cd"]   <- "PCD"

# drop noregistrados, nulos
df1996$noregistrados <- NULL
df1996$nulos         <- NULL

# generate uniqueid
df1996$uniqueid <- 0
df1996$uniqueid[df1996$municipality == "ACAPONETA"]          <- 18001
df1996$uniqueid[df1996$municipality == "AHUACATLAN"]         <- 18002
df1996$uniqueid[df1996$municipality == "AMATLAN DE CANAS"]   <- 18003
df1996$uniqueid[df1996$municipality == "BAHIA DE BANDERAS"]  <- 18020
df1996$uniqueid[df1996$municipality == "COMPOSTELA"]         <- 18004
df1996$uniqueid[df1996$municipality == "EL NAYAR"]           <- 18009
df1996$uniqueid[df1996$municipality == "HUAJICORI"]          <- 18005
df1996$uniqueid[df1996$municipality == "IXTLAN DEL RIO"]     <- 18006
df1996$uniqueid[df1996$municipality == "JALA"]               <- 18007
df1996$uniqueid[df1996$municipality == "LA YESCA"]           <- 18019
df1996$uniqueid[df1996$municipality == "ROSAMORADA"]         <- 18010
df1996$uniqueid[df1996$municipality == "RUIZ"]               <- 18011
df1996$uniqueid[df1996$municipality == "SAN BLAS"]           <- 18012
df1996$uniqueid[df1996$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df1996$uniqueid[df1996$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df1996$uniqueid[df1996$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df1996$uniqueid[df1996$municipality == "TECUALA"]            <- 18016
df1996$uniqueid[df1996$municipality == "TEPIC"]              <- 18017
df1996$uniqueid[df1996$municipality == "TUXPAN"]             <- 18018
df1996$uniqueid[df1996$municipality == "XALISCO"]            <- 18008

# generate valid
df1996$valid <- rowSums(df1996[, c("PAN","PRI","PRD","PT","PVEM","PRS","PARM","PCD")],
                        na.rm = TRUE)

# (Omit municipal aggregator, rowranks, winner lines)

# add year/month
df1996$year  <- 1996
df1996$month <- "July"

# We can save to an intermediate file or keep it in memory. Let's keep in memory.

# Keep a small subset for merging with 1999
df1996_merge <- df1996[, c("municipality","section","uniqueid")]
df1996_merge <- df1996_merge[order(df1996_merge$section), ]

# Also, drop if total == NA or total==0 
df1996 <- subset(df1996, !is.na(total) & total != 0)

###############################################################################
## 3) 1999 DATA
###############################################################################
df1999 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_1999_No_LN.csv", stringsAsFactors = FALSE)

# Remove upper, "-" and spaces

colnames(df1999) <- tolower(colnames(df1999))
names(df1999) <- gsub("[. ]", "", names(df1999))

names(df1999)[names(df1999) == "nomunicipio"] <- "municipality"
names(df1999)[names(df1999) == "seccion"]     <- "section"

df1999 <- subset(df1999, !(is.na(municipality) & is.na(section)))

df1999$total <- rowSums(df1999[, c("pri","pvem","medp","parmen","pps","cac","candnoreg","votosnulos")],
                        na.rm = TRUE)
df1999 <- subset(df1999, !is.na(total) & total != 0)

num_vars_99 <- c("pri","pvem","medp","parmen","pps","cac","candnoreg","votosnulos","total")
for(v in num_vars_99){
  if(v %in% colnames(df1999)){
    df1999[[v]] <- as.numeric(df1999[[v]])
  }
}

# omit "collapse" -> keep municipality-section level
# rename as if collapsed
names(df1999)[names(df1999) == "cac"]    <- "PAN_PRD_PT"
names(df1999)[names(df1999) == "pri"]    <- "PRI"
names(df1999)[names(df1999) == "pvem"]   <- "PVEM"
names(df1999)[names(df1999) == "medp"]   <- "PMEP"
names(df1999)[names(df1999) == "parmen"] <- "PARM"
names(df1999)[names(df1999) == "pps"]    <- "PPS"

df1999$candnoreg  <- NULL
df1999$votosnulos <- NULL

df1999$valid <- rowSums(df1999[, c("PRI","PVEM","PMEP","PARM","PPS","PAN_PRD_PT")], na.rm = TRUE)

# Merge with all_months_years
ln_all_months_years <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/ln_all_months_years.dta")

ln_all_months_years <- ln_all_months_years %>% 
  dplyr::filter(state == "NAYARIT" & month == "March" & year == 1999)

df1999$ed      <- 18
df1999$seccion <- df1999$section

# Merge the datasets
df1999 <- df1999 %>%
  dplyr::left_join(ln_all_months_years %>% dplyr::select(section,lista), by = c("section")) %>% 
  dplyr::mutate(listanominal = lista) %>% 
  dplyr::select(-lista)

df1999$turnout <- df1999$total / df1999$listanominal

df1999$year  <- 1999
df1999$month <- "July"

# drop municipality for the next merge
temp_merged_99$municipality <- NULL

df1999 <- subset(df1999, !(is.na(df1999$uniqueid) & is.na(df1999$municipality)))

# Sort by section
df1999 <- df1999[order(df1999$section), ]

###############################################################################
## 4) 2002 DATA
###############################################################################
df2002 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_2002_No_LN.csv", stringsAsFactors = FALSE)
# Remove upper, "-" and spaces

colnames(df2002) <- tolower(colnames(df2002))
names(df2002) <- gsub("[. ]", "", names(df2002))

names(df2002)[names(df2002) == "nombre_municipio"] <- "municipality"
names(df2002)[names(df2002) == "seccion"]          <- "section"

df2002 <- subset(df2002, !(municipality == "" & is.na(section)))

# We'll do it carefully, but first ensure 'total' is numeric:
df2002$total <- as.numeric(df2002$total)
df2002 <- subset(df2002, !is.na(total) & total != 0)

num_vars_02 <- c("pan","pri","prd","pt","pvem","prs","medp","cdppn","psn","pas","total")
for(v in num_vars_02){
  if(v %in% colnames(df2002)){
    df2002[[v]] <- as.numeric(df2002[[v]])
  }
}

# rename columns to final names
names(df2002)[names(df2002) == "pan"]   <- "PAN"
names(df2002)[names(df2002) == "pri"]   <- "PRI"
names(df2002)[names(df2002) == "prd"]   <- "PRD"
names(df2002)[names(df2002) == "pt"]    <- "PT"
names(df2002)[names(df2002) == "pvem"]  <- "PVEM"
names(df2002)[names(df2002) == "prs"]   <- "PRS"
names(df2002)[names(df2002) == "medp"]  <- "PMEP"
names(df2002)[names(df2002) == "cdppn"] <- "PC"
names(df2002)[names(df2002) == "psn"]   <- "PSN"
names(df2002)[names(df2002) == "pas"]   <- "PAS"

# uniqueid
df2002$uniqueid <- 0
df2002$uniqueid[df2002$municipality == "ACAPONETA"]           <- 18001
df2002$uniqueid[df2002$municipality == "AHUACATLAN"]          <- 18002
df2002$uniqueid[df2002$municipality == "AMATLAN DE CA?AS"]    <- 18003
df2002$uniqueid[df2002$municipality == "BAHIA DE BANDERAS"]   <- 18020
df2002$uniqueid[df2002$municipality == "COMPOSTELA"]          <- 18004
df2002$uniqueid[df2002$municipality == "EL NAYAR"]            <- 18009
df2002$uniqueid[df2002$municipality == "HUAJICORI"]           <- 18005
df2002$uniqueid[df2002$municipality == "IXTLAN DEL RIO"]      <- 18006
df2002$uniqueid[df2002$municipality == "JALA"]                <- 18007
df2002$uniqueid[df2002$municipality == "LA YESCA"]            <- 18019
df2002$uniqueid[df2002$municipality == "ROSAMORADA"]          <- 18010
df2002$uniqueid[df2002$municipality == "RUIZ"]                <- 18011
df2002$uniqueid[df2002$municipality == "SAN BLAS"]            <- 18012
df2002$uniqueid[df2002$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2002$uniqueid[df2002$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2002$uniqueid[df2002$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2002$uniqueid[df2002$municipality == "TECUALA"]             <- 18016
df2002$uniqueid[df2002$municipality == "TEPIC"]               <- 18017
df2002$uniqueid[df2002$municipality == "TUXPAN"]              <- 18018
df2002$uniqueid[df2002$municipality == "XALISCO"]             <- 18008

df2002$valid <- rowSums(df2002[, c("PAN","PRI","PRD","PT","PVEM","PRS","PMEP","PC","PSN","PAS")], na.rm = TRUE)

# Merge with all_months_years
df2002$ed      <- 18
df2002$seccion <- df2002$section

temp_2002 <- merge(df2002,
                   all_my[, c("ed","seccion","month","year","lista")],
                   by.x = c("ed","seccion"), 
                   by.y = c("ed","seccion"),
                   all.x = TRUE, 
                   all.y = FALSE)

temp_2002 <- subset(temp_2002, month == 6 & year == 2002)
temp_2002 <- subset(temp_2002, !(is.na(month)))

temp_2002$ed      <- NULL
temp_2002$seccion <- NULL
temp_2002$month   <- NULL
temp_2002$year    <- NULL

names(temp_2002)[names(temp_2002) == "lista"] <- "listanominal"
temp_2002$turnout <- temp_2002$total / temp_2002$listanominal

temp_2002$year  <- 2002
temp_2002$month <- "July"

temp_2002 <- temp_2002[order(temp_2002$section), ]

###############################################################################
## 5) 2005 DATA
###############################################################################
df2005 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_2005_No_LN.csv", stringsAsFactors = FALSE)
# Remove upper, "-" and spaces

colnames(df2005) <- tolower(colnames(df2005))
names(df2005) <- gsub("[. ]", "", names(df2005))

names(df2005)[names(df2005) == "nombre_municipio"] <- "municipality"
names(df2005)[names(df2005) == "seccion"]          <- "section"

df2005 <- subset(df2005, !(municipality == "" & is.na(section)))

df2005$total <- as.numeric(df2005$total)
df2005 <- subset(df2005, !is.na(total) & total != 0)

num_vars_05 <- c("pan","pri","prdptprs","pc","pvem","total")
for(v in num_vars_05){
  if(v %in% colnames(df2005)){
    df2005[[v]] <- as.numeric(df2005[[v]])
  }
}

# rename
names(df2005)[names(df2005) == "pan"]      <- "PAN"
names(df2005)[names(df2005) == "pri"]      <- "PRI"
names(df2005)[names(df2005) == "prdptprs"] <- "PRD_PT_PRS"
names(df2005)[names(df2005) == "pc"]       <- "PC"
names(df2005)[names(df2005) == "pvem"]     <- "PVEM"

df2005$uniqueid <- 0
df2005$uniqueid[df2005$municipality == "ACAPONETA"]           <- 18001
df2005$uniqueid[df2005$municipality == "AHUACATLAN"]          <- 18002
df2005$uniqueid[df2005$municipality == "AMATLAN DE CA?AS"]    <- 18003
df2005$uniqueid[df2005$municipality == "BAHIA DE BANDERAS"]   <- 18020
df2005$uniqueid[df2005$municipality == "COMPOSTELA"]          <- 18004
df2005$uniqueid[df2005$municipality == "EL NAYAR"]            <- 18009
df2005$uniqueid[df2005$municipality == "HUAJICORI"]           <- 18005
df2005$uniqueid[df2005$municipality == "IXTLAN DEL RIO"]      <- 18006
df2005$uniqueid[df2005$municipality == "JALA"]                <- 18007
df2005$uniqueid[df2005$municipality == "LA YESCA"]            <- 18019
df2005$uniqueid[df2005$municipality == "ROSAMORADA"]          <- 18010
df2005$uniqueid[df2005$municipality == "RUIZ"]                <- 18011
df2005$uniqueid[df2005$municipality == "SAN BLAS"]            <- 18012
df2005$uniqueid[df2005$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2005$uniqueid[df2005$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2005$uniqueid[df2005$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2005$uniqueid[df2005$municipality == "TECUALA"]             <- 18016
df2005$uniqueid[df2005$municipality == "TEPIC"]               <- 18017
df2005$uniqueid[df2005$municipality == "TUXPAN"]              <- 18018
df2005$uniqueid[df2005$municipality == "XALISCO"]             <- 18008

df2005$valid <- rowSums(df2005[, c("PAN","PRI","PVEM","PC","PRD_PT_PRS")], na.rm = TRUE)

df2005$ed      <- 18
df2005$seccion <- df2005$section
temp_2005 <- merge(df2005,
                   all_my[, c("ed","seccion","month","year","lista")],
                   by.x = c("ed","seccion"), 
                   by.y = c("ed","seccion"),
                   all.x = TRUE, 
                   all.y = FALSE)
temp_2005 <- subset(temp_2005, month == 6 & year == 2005)
temp_2005 <- subset(temp_2005, !(is.na(month)))

temp_2005$ed      <- NULL
temp_2005$seccion <- NULL
temp_2005$month   <- NULL
temp_2005$year    <- NULL

names(temp_2005)[names(temp_2005) == "lista"] <- "listanominal"
temp_2005$turnout <- temp_2005$total / temp_2005$listanominal

temp_2005$year  <- 2005
temp_2005$month <- "July"
temp_2005 <- temp_2005[order(temp_2005$section), ]

###############################################################################
## 6) 2008 DATA
###############################################################################
df2008 <- read.csv("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_2008.csv", stringsAsFactors = FALSE)

names(df2008)[names(df2008) == "nombre_municipio"] <- "municipality"
names(df2008)[names(df2008) == "seccion"]          <- "section"
names(df2008)[names(df2008) == "lista_nominal"]    <- "listanominal"

df2008 <- subset(df2008, !(municipality == "" & is.na(section)))

df2008$total <- as.numeric(df2008$total)
df2008 <- subset(df2008, !is.na(total) & total != 0)

num_vars_08 <- c("listanominal","pan","pripanal","prdpvem","pt","pcprs","pas","total")
for(v in num_vars_08){
  if(v %in% colnames(df2008)){
    df2008[[v]] <- as.numeric(df2008[[v]])
  }
}

names(df2008)[names(df2008) == "pan"]     <- "PAN"
names(df2008)[names(df2008) == "pripanal"]<- "PRI_PANAL"
names(df2008)[names(df2008) == "prdpvem"] <- "PRD_PVEM"
names(df2008)[names(df2008) == "pt"]      <- "PT"
names(df2008)[names(df2008) == "pcprs"]   <- "PC_PRS"
names(df2008)[names(df2008) == "pas"]     <- "PAS"

df2008$turnout <- df2008$total / df2008$listanominal

df2008$uniqueid <- 0
df2008$uniqueid[df2008$municipality == "ACAPONETA"]           <- 18001
df2008$uniqueid[df2008$municipality == "AHUACATLAN"]          <- 18002
df2008$uniqueid[df2008$municipality == "AMATLAN DE CA?AS"]    <- 18003
df2008$uniqueid[df2008$municipality == "BAHIA DE BANDERAS"]   <- 18020
df2008$uniqueid[df2008$municipality == "COMPOSTELA"]          <- 18004
df2008$uniqueid[df2008$municipality == "EL NAYAR"]            <- 18009
df2008$uniqueid[df2008$municipality == "HUAJICORI"]           <- 18005
df2008$uniqueid[df2008$municipality == "IXTLAN DEL RIO"]      <- 18006
df2008$uniqueid[df2008$municipality == "JALA"]                <- 18007
df2008$uniqueid[df2008$municipality == "LA YESCA"]            <- 18019
df2008$uniqueid[df2008$municipality == "ROSAMORADA"]          <- 18010
df2008$uniqueid[df2008$municipality == "RUIZ"]                <- 18011
df2008$uniqueid[df2008$municipality == "SAN BLAS"]            <- 18012
df2008$uniqueid[df2008$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2008$uniqueid[df2008$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2008$uniqueid[df2008$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2008$uniqueid[df2008$municipality == "TECUALA"]             <- 18016
df2008$uniqueid[df2008$municipality == "TEPIC"]               <- 18017
df2008$uniqueid[df2008$municipality == "TUXPAN"]              <- 18018
df2008$uniqueid[df2008$municipality == "XALISCO"]             <- 18008

df2008$valid <- rowSums(df2008[, c("PAN","PT","PAS","PRD_PVEM","PC_PRS","PRI_PANAL")],
                        na.rm = TRUE)

df2008$year  <- 2008
df2008$month <- "July"
df2008 <- df2008[order(df2008$section), ]

###############################################################################
## 7) 2011 DATA
###############################################################################
df2011 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayu_Seccion_2011.xlsx", sheet = "Sheet1")

# drop K, L
df2011 <- df2011 %>% select(-c(K, L))

names(df2011)[names(df2011) == "MUNICIPIO"] <- "municipality"
names(df2011)[names(df2011) == "Section"]   <- "section"

# fill municipality if blank
df2011 <- df2011 %>%
  tidyr::fill(municipality, .direction = "down")

df2011$total <- rowSums(df2011[, c("PAN","PRD","PRS","PRI_PVEM_PANAL","PT_PC",
                                   "CandidatosNoRegistrados","VotosNulos")],
                        na.rm = TRUE)
df2011 <- subset(df2011, !(is.na(total) | total == 0))
df2011$CandidatosNoRegistrados <- NULL
df2011$VotosNulos             <- NULL

df2011$municipality <- trimws(df2011$municipality)

df2011$uniqueid <- 0
df2011$uniqueid[df2011$municipality == "ACAPONETA"]          <- 18001
df2011$uniqueid[df2011$municipality == "AHUACATLAN"]         <- 18002
df2011$uniqueid[df2011$municipality == "AMATLAN DE CANAS"]   <- 18003
df2011$uniqueid[df2011$municipality == "BAHIA DE BANDERAS"]  <- 18020
df2011$uniqueid[df2011$municipality == "COMPOSTELA"]         <- 18004
df2011$uniqueid[df2011$municipality == "EL NAYAR"]           <- 18009
df2011$uniqueid[df2011$municipality == "HUAJICORI"]          <- 18005
df2011$uniqueid[df2011$municipality == "IXTLAN DEL RIO"]     <- 18006
df2011$uniqueid[df2011$municipality == "JALA"]               <- 18007
df2011$uniqueid[df2011$municipality == "LA YESCA"]           <- 18019
df2011$uniqueid[df2011$municipality == "ROSAMORADA"]         <- 18010
df2011$uniqueid[df2011$municipality == "RUIZ"]               <- 18011
df2011$uniqueid[df2011$municipality == "SAN BLAS"]           <- 18012
df2011$uniqueid[df2011$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df2011$uniqueid[df2011$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df2011$uniqueid[df2011$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df2011$uniqueid[df2011$municipality == "TECUALA"]            <- 18016
df2011$uniqueid[df2011$municipality == "TEPIC"]              <- 18017
df2011$uniqueid[df2011$municipality == "TUXPAN"]             <- 18018
df2011$uniqueid[df2011$municipality == "XALISCO"]            <- 18008

df2011$valid <- rowSums(df2011[, c("PAN","PRD","PRS","PRI_PVEM_PANAL","PT_PC")], na.rm = TRUE)

# Merge with all_months_years
df2011$ed      <- 18
df2011$seccion <- df2011$section

temp_2011 <- merge(df2011,
                   all_my[, c("ed","seccion","month","year","lista")],
                   by.x = c("ed","seccion"),
                   by.y = c("ed","seccion"),
                   all.x = TRUE,
                   all.y = FALSE)
temp_2011 <- subset(temp_2011, month == 6 & year == 2011)
temp_2011 <- subset(temp_2011, !(is.na(month)))

temp_2011$ed      <- NULL
temp_2011$seccion <- NULL
temp_2011$month   <- NULL
temp_2011$year    <- NULL

names(temp_2011)[names(temp_2011) == "lista"] <- "listanominal"
temp_2011$turnout <- temp_2011$total / temp_2011$listanominal

temp_2011$year  <- 2011
temp_2011$month <- "July"
temp_2011 <- temp_2011[order(temp_2011$section), ]

###############################################################################
## 8) APPEND 1996–2011 DATA
###############################################################################

df_96_99 <- rbind(
  df1996,
  df1999
)

df_96_99_02 <- rbind(
  df_96_99,
  temp_2002
)

df_96_99_02_05 <- rbind(
  df_96_99_02,
  temp_2005
)

df_96_99_02_05_08 <- rbind(
  df_96_99_02_05,
  df2008
)

df_96_99_02_05_08_11 <- rbind(
  df_96_99_02_05_08,
  temp_2011
)

# This dataset is the 1996–2011 combined:
df_1996_2011 <- df_96_99_02_05_08_11

###############################################################################
## 9) 2014 DATA
###############################################################################

excel_path_2014 <- "../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayuntamientos_2014.xlsx"
sheet_names_2014 <- excel_sheets(excel_path_2014)

# Read each sheet into a list
list_of_dfs_2014 <- lapply(sheet_names_2014, function(sn) {
  read_excel(excel_path_2014, sheet = sn, col_types = "text")
})

# Combine them
df2014 <- bind_rows(list_of_dfs_2014)

# Keep if municipality != ""
df2014 <- subset(df2014, municipality != "" & !is.na(municipality))

# Convert known numeric columns if needed; for example:
# Suppose these columns exist: Sección, etc.
names(df2014)[names(df2014) == "Sección"]       <- "section"
names(df2014)[names(df2014) == "CandidatoNoReg"]<- "no_reg"
names(df2014)[names(df2014) == "VotosNulos"]    <- "nulo"

df2014$uniqueid <- 0
df2014$uniqueid[df2014$municipality == "ACAPONETA"]           <- 18001
df2014$uniqueid[df2014$municipality == "AHUACATLÁN"]          <- 18002
df2014$uniqueid[df2014$municipality == "AMATLÁN DE CAÑAS"]    <- 18003
df2014$uniqueid[df2014$municipality == "BAHÍA DE BANDERAS"]   <- 18020
df2014$uniqueid[df2014$municipality == "COMPOSTELA"]          <- 18004
df2014$uniqueid[df2014$municipality == "DEL NAYAR"]           <- 18009
df2014$uniqueid[df2014$municipality == "HUAJICORI"]           <- 18005
df2014$uniqueid[df2014$municipality == "IXTLÁN DEL RÍO"]      <- 18006
df2014$uniqueid[df2014$municipality == "JALA"]                <- 18007
df2014$uniqueid[df2014$municipality == "LA YESCA"]            <- 18019
df2014$uniqueid[df2014$municipality == "ROSAMORADA"]          <- 18010
df2014$uniqueid[df2014$municipality == "RUÍZ"]                <- 18011
df2014$uniqueid[df2014$municipality == "SAN BLAS"]            <- 18012
df2014$uniqueid[df2014$municipality == "SAN PEDRO LAGUNILLAS"]<- 18013
df2014$uniqueid[df2014$municipality == "SANTA MARIA DEL ORO"] <- 18014
df2014$uniqueid[df2014$municipality == "SANTIAGO IXCUINTLA"]  <- 18015
df2014$uniqueid[df2014$municipality == "TECUALA"]             <- 18016
df2014$uniqueid[df2014$municipality == "TEPIC"]               <- 18017
df2014$uniqueid[df2014$municipality == "TUXPAN"]              <- 18018
df2014$uniqueid[df2014$municipality == "XALISCO"]             <- 18008

# We'll skip that aggregator step.

# Add year/month/state
df2014$year  <- 2014
df2014$month <- "July"
df2014$STATE <- "NAYARIT"

# This is your 2014 municipality-section data.
# We keep it at that level, skipping rowranks/winner.

###############################################################################
## 10) 2017 DATA
###############################################################################
df2017 <- read_excel("../../../Data/Raw Electoral Data/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011,2014,2017/Ayuntamientos_2017.xlsx", sheet = "AYUNTAMIENTOS", col_types = "text")

df2017$uniqueid <- 0
df2017$uniqueid[df2017$municipality == "ACAPONETA"]          <- 18001
df2017$uniqueid[df2017$municipality == "AHUACATLAN"]         <- 18002
df2017$uniqueid[df2017$municipality == "AMATLAN DE CAÑAS"]   <- 18003
df2017$uniqueid[df2017$municipality == "BAHIA DE BANDERAS"]  <- 18020
df2017$uniqueid[df2017$municipality == "COMPOSTELA"]         <- 18004
df2017$uniqueid[df2017$municipality == "DEL NAYAR"]          <- 18009
df2017$uniqueid[df2017$municipality == "HUAJICORI"]          <- 18005
df2017$uniqueid[df2017$municipality == "IXTLAN DEL RIO"]     <- 18006
df2017$uniqueid[df2017$municipality == "JALA"]               <- 18007
df2017$uniqueid[df2017$municipality == "LA YESCA"]           <- 18019
df2017$uniqueid[df2017$municipality == "ROSAMORADA"]         <- 18010
df2017$uniqueid[df2017$municipality == "RUIZ"]               <- 18011
df2017$uniqueid[df2017$municipality == "SAN BLAS"]           <- 18012
df2017$uniqueid[df2017$municipality == "SAN PEDRO LAGUNILLAS"] <- 18013
df2017$uniqueid[df2017$municipality == "SANTA MARIA DEL ORO"]  <- 18014
df2017$uniqueid[df2017$municipality == "SANTIAGO IXCUINTLA"]   <- 18015
df2017$uniqueid[df2017$municipality == "TECUALA"]            <- 18016
df2017$uniqueid[df2017$municipality == "TEPIC"]              <- 18017
df2017$uniqueid[df2017$municipality == "TUXPAN"]             <- 18018
df2017$uniqueid[df2017$municipality == "XALISCO"]            <- 18008

# drop columns not needed if they exist
# skip aggregator or rowranks or winner
df2017$year  <- 2017
df2017$month <- "June"
df2017$STATE <- "NAYARIT"

###############################################################################
## 11) APPEND (2014, 2017) + (1996–2011)
###############################################################################
# We'll append 2014 and 2017 together, then append them to the 1996–2011.

df_14_17 <- bind_rows(df2014, df2017)

# Then combine with older data:
df_final <- bind_rows(df_1996_2011, df_14_17)

###############################################################################
## 12) SAVE FINAL DATASET
###############################################################################
# Now df_final is the full municipality-section panel for 1996–2011 + 2014 + 2017
# at the municipality-section level, skipping aggregator, ranking, and winner.

data.table::fwrite(df_final,"../../../Processed Data/nayarit/nayarit_process_raw_data.csv")


