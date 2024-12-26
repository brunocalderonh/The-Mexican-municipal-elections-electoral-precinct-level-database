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
# 1) Read "Lisado Nominal 2004.xlsx" 
################################################################################


df_listanom <- read_excel(
  path = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Lisado Nominal 2004.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()

# keep only (section, listanominal)
df_listanom <- df_listanom %>%
  select(section, listanominal)

# drop if section==. & listanominal==.
df_listanom <- df_listanom %>%
  filter(!(is.na(section) & is.na(listanominal)))

# sort section => in R we do:
df_listanom <- df_listanom %>%
  arrange(section)

################################################################################
# 2) Read "Ayu_Seccion_2004_No_LN.csv" (Equivalent to insheet),
#    rename columns, drop if municipality=="" & section missing,
#    destring columns pan-nulos, collapse by (municipality, section)
################################################################################
df_ayusec <- read_csv("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2004_No_LN.csv", show_col_types = FALSE) 
colnames(df_ayusec) <- tolower(colnames(df_ayusec))
names(df_ayusec) <- gsub("[- ]", "", names(df_ayusec))

df_ayusec <-df_ayusec %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section)))

# destring: in R we parse as numeric
df_ayusec <- df_ayusec %>%
  mutate(across(c(pan:nulos), as.numeric))

# collapse (sum) pan - nulos by (municipality, section)
df_collapsed <- df_ayusec %>%
  group_by(municipality, section) %>%
  summarise(across(pan:nulos, sum, na.rm=TRUE), .groups="drop")

# generate total = rowtotal(pan fide prd prv no_reg nulos)
# drop if total==. or total==0
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(total = sum(c_across(c(pan, fide, prd, prv, no_reg, nulos)), na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!(is.na(total) | total==0))

# rename columns
df_collapsed <- df_collapsed %>%
  rename(
    PAN          = pan,
    PRI_PVEM     = fide,
    PRD_PT_PC    = prd,   # per code: rename prd => PRD_PT_PC
    PRV          = prv
  ) %>%
  # drop nulos, no_reg if present
  select(-nulos, -no_reg)

################################################################################
# 3) Merge with "Listanominal"
################################################################################


# sort by section => in R we can do:
df_collapsed <- df_collapsed %>%
  arrange(section)

# Merge on 'section'
df_merged <- df_collapsed %>%
  left_join(df_listanom, by="section")

# drop if _merge==2 => in R, that means dropping rows with no match in 'listanominal'
df_merged <- df_merged %>%
  filter(!is.na(listanominal))

################################################################################
# 4) gen turnout = total/listanominal
#    drop nulos, no_reg if not already done
################################################################################
df_merged <- df_merged %>%
  mutate(turnout = total / listanominal)

################################################################################
# 5) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_merged <- df_merged %>%
  mutate(uniqueid=0) %>%
  mutate(uniqueid= case_when(
    municipality == "ACAJETE"                  ~ 30001,
    municipality == "ACATLAN"                  ~ 30002,
    municipality == "ACAYUCAN"                 ~ 30003,
    municipality == "ACTOPAN"                  ~ 30004,
    municipality == "ACULA"                    ~ 30005,
    municipality == "ACULTZINGO"               ~ 30006,
    municipality == "AGUA DULCE"               ~ 30204,
    municipality == "TEMAPACHE"                ~ 30160,
    municipality == "ALPATLAHUAC"              ~ 30008,
    municipality == "ALTO LUCERO"              ~ 30009,
    municipality == "ALTOTONGA"                ~ 30010,
    municipality == "ALVARADO"                 ~ 30011,
    municipality == "AMATITLAN"                ~ 30012,
    municipality == "AMATLAN DE LOS REYES"     ~ 30014,
    municipality == "ANGEL R. CABADA"          ~ 30015,
    municipality == "APAZAPAN"                 ~ 30017,
    municipality == "AQUILA"                   ~ 30018,
    municipality == "ASTACINGA"                ~ 30019,
    municipality == "ATLAHUILCO"               ~ 30020,
    municipality == "ATOYAC"                   ~ 30021,
    municipality == "ATZACAN"                  ~ 30022,
    municipality == "ATZALAN"                  ~ 30023,
    municipality == "AYAHUALULCO"              ~ 30025,
    municipality == "BANDERILLA"               ~ 30026,
    municipality == "BENITO JUAREZ"            ~ 30027,
    municipality == "BOCA DEL RIO"             ~ 30028,
    municipality == "CALCAHUALCO"              ~ 30029,
    municipality == "CAMARON DE TEJEDA"        ~ 30007,
    municipality == "CAMERINO Z. MENDOZA"      ~ 30030,
    municipality == "CARLOS A. CARRILLO"       ~ 30208,
    municipality == "CARRILLO PUERTO"          ~ 30031,
    municipality == "CASTILLO DE TEAYO"        ~ 30157,
    municipality == "CATEMACO"                 ~ 30032,
    municipality == "CAZONES DE HERRERA"       ~ 30033,
    municipality == "CERRO AZUL"               ~ 30034,
    municipality == "CHACALTIANGUIS"           ~ 30054,
    municipality == "CHALMA"                   ~ 30055,
    municipality == "CHICONAMEL"               ~ 30056,
    municipality == "CHICONQUIACO"             ~ 30057,
    municipality == "CHICONTEPEC"              ~ 30058,
    municipality == "CHINAMECA"                ~ 30059,
    municipality == "CHINAMPA DE GOROSTIZA"    ~ 30060,
    municipality == "CHOCAMAN"                 ~ 30062,
    municipality == "CHONTLA"                  ~ 30063,
    municipality == "CHUMATLAN"                ~ 30064,
    municipality == "CITLALTEPETL"             ~ 30035,
    municipality == "COACOATZINTLA"            ~ 30036,
    municipality == "COAHUITLAN"               ~ 30037,
    municipality == "COATEPEC"                 ~ 30038,
    municipality == "COATZACOALCOS"            ~ 30039,
    municipality == "COATZINTLA"               ~ 30040,
    municipality == "COETZALA"                 ~ 30041,
    municipality == "COLIPA"                   ~ 30042,
    municipality == "COMAPA"                   ~ 30043,
    municipality == "CORDOBA"                  ~ 30044,
    municipality == "COSAMALOAPAN"             ~ 30045,
    municipality == "COSAUTLAN DE CARVAJAL"    ~ 30046,
    municipality == "COSCOMATEPEC"             ~ 30047,
    municipality == "COSOLEACAQUE"             ~ 30048,
    municipality == "COTAXTLA"                 ~ 30049,
    municipality == "COXQUIHUI"                ~ 30050,
    municipality == "COYUTLA"                  ~ 30051,
    municipality == "CUICHAPA"                 ~ 30052,
    municipality == "CUITLAHUAC"               ~ 30053,
    municipality == "EL HIGO"                  ~ 30205,
    municipality == "EMILIANO ZAPATA"          ~ 30065,
    municipality == "ESPINAL"                  ~ 30066,
    municipality == "FILOMENO MATA"            ~ 30067,
    municipality == "FORTIN"                   ~ 30068,
    municipality == "GUTIERREZ ZAMORA"         ~ 30069,
    municipality == "HIDALGOTITLAN"            ~ 30070,
    municipality == "HUATUSCO"                 ~ 30071,
    municipality == "HUAYACOCOTLA"             ~ 30072,
    municipality == "HUEYAPAN DE OCAMPO"       ~ 30073,
    municipality == "HUILOAPAN DE CUAUHTEMOC"  ~ 30074,
    municipality == "IGNACIO DE LA LLAVE"      ~ 30075,
    municipality == "ILAMATLAN"                ~ 30076,
    municipality == "ISLA"                     ~ 30077,
    municipality == "IXCATEPEC"                ~ 30078,
    municipality == "IXHUACAN DE LOS REYES"    ~ 30079,
    municipality == "IXHUATLAN DE MADERO"      ~ 30083,
    municipality == "IXHUATLAN DEL CAFE"       ~ 30080,
    municipality == "IXHUATLAN DEL SURESTE"    ~ 30082,
    municipality == "IXHUATLANCILLO"           ~ 30081,
    municipality == "IXMATLAHUACAN"            ~ 30084,
    municipality == "IXTACZOQUITLAN"           ~ 30085,
    municipality == "JALACINGO"                ~ 30086,
    municipality == "JALCOMULCO"               ~ 30088,
    municipality == "JALTIPAN"                 ~ 30089,
    municipality == "JAMAPA"                   ~ 30090,
    municipality == "JESUS CARRANZA"           ~ 30091,
    municipality == "JILOTEPEC"                ~ 30093,
    municipality == "JOSE AZUETA"              ~ 30169,
    municipality == "JUAN RODRIGUEZ CLARA"     ~ 30094,
    municipality == "JUCHIQUE DE FERRER"       ~ 30095,
    municipality == "LA ANTIGUA"               ~ 30016,
    municipality == "LA PERLA"                 ~ 30127,
    municipality == "LANDERO Y COSS"           ~ 30096,
    municipality == "LAS CHOAPAS"              ~ 30061,
    municipality == "LAS MINAS"                ~ 30107,
    municipality == "LAS VIGAS DE RAMIREZ"     ~ 30132,
    municipality == "LERDO DE TEJADA"          ~ 30097,
    municipality == "LOS REYES"                ~ 30137,
    municipality == "MAGDALENA"                ~ 30098,
    municipality == "MALTRATA"                 ~ 30099,
    municipality == "MANLIO FABIO ALTAMIRANO"  ~ 30100,
    municipality == "MARIANO ESCOBEDO"         ~ 30101,
    municipality == "MARTINEZ DE LA TORRE"     ~ 30102,
    municipality == "MECATLAN"                 ~ 30103,
    municipality == "MECAYAPAN"                ~ 30104,
    municipality == "MEDELLIN"                 ~ 30105,
    municipality == "MIAHUATLAN"               ~ 30106,
    municipality == "MINATITLAN"               ~ 30108,
    municipality == "MISANTLA"                 ~ 30109,
    municipality == "MIXTLA DE ALTAMIRANO"     ~ 30110,
    municipality == "MOLOACAN"                 ~ 30111,
    municipality == "NANCHITAL DE L.C. DEL RIO"~ 30206,
    municipality == "NAOLINCO"                 ~ 30112,
    municipality == "NARANJAL"                 ~ 30113,
    municipality == "NARANJOS AMATLAN"         ~ 30013,
    municipality == "NAUTLA"                   ~ 30114,
    municipality == "NOGALES"                  ~ 30115,
    municipality == "OLUTA"                    ~ 30116,
    municipality == "OMEALCA"                  ~ 30117,
    municipality == "ORIZABA"                  ~ 30118,
    municipality == "OTATITLAN"                ~ 30119,
    municipality == "OTEAPAN"                  ~ 30120,
    municipality == "OZULUAMA"                 ~ 30121,
    municipality == "PAJAPAN"                  ~ 30122,
    municipality == "PANUCO"                   ~ 30123,
    municipality == "PAPANTLA"                 ~ 30124,
    municipality == "PASO DEL MACHO"           ~ 30125,
    municipality == "PASO DE OVEJAS"           ~ 30126,
    municipality == "PEROTE"                   ~ 30128,
    municipality == "PLATON SANCHEZ"           ~ 30129,
    municipality == "PLAYA VICENTE"            ~ 30130,
    municipality == "POZA RICA"                ~ 30131,
    municipality == "PUEBLO VIEJO"             ~ 30133,
    municipality == "PUENTE NACIONAL"          ~ 30134,
    municipality == "RAFAEL DELGADO"           ~ 30135,
    municipality == "RAFAEL LUCIO"             ~ 30136,
    municipality == "RIO BLANCO"               ~ 30138,
    municipality == "SALTABARRANCA"            ~ 30139,
    municipality == "SAN ANDRES TENEJAPAN"     ~ 30140,
    municipality == "SAN ANDRES TUXTLA"        ~ 30141,
    municipality == "SAN JUAN EVANGELISTA"     ~ 30142,
    municipality == "SAN RAFAEL"               ~ 30211,
    municipality == "SANTIAGO SOCHIAPA"        ~ 30212,
    municipality == "SANTIAGO TUXTLA"          ~ 30143,
    municipality == "SAYULA DE ALEMAN"         ~ 30144,
    municipality == "SOCHIAPA"                 ~ 30146,
    municipality == "SOCONUSCO"                ~ 30145,
    municipality == "SOLEDAD ATZOMPA"          ~ 30147,
    municipality == "SOLEDAD DE DOBLADO"       ~ 30148,
    municipality == "SOTEAPAN"                 ~ 30149,
    municipality == "TAMALIN"                  ~ 30150,
    municipality == "TAMIAHUA"                 ~ 30151,
    municipality == "TAMPICO ALTO"             ~ 30152,
    municipality == "TANCOCO"                  ~ 30153,
    municipality == "TANTIMA"                  ~ 30154,
    municipality == "TANTOYUCA"                ~ 30155,
    municipality == "TATAHUICAPAN"             ~ 30209,
    municipality == "TATATILA"                 ~ 30156,
    municipality == "TECOLUTLA"                ~ 30158,
    municipality == "TEHUIPANGO"               ~ 30159,
    municipality == "TEMPOAL"                  ~ 30161,
    municipality == "TENAMPA"                  ~ 30162,
    municipality == "TENOCHTITLAN"             ~ 30163,
    municipality == "TEOCELO"                  ~ 30164,
    municipality == "TEPATLAXCO"               ~ 30165,
    municipality == "TEPETLAN"                 ~ 30166,
    municipality == "TEPETZINTLA"              ~ 30167,
    municipality == "TEQUILA"                  ~ 30168,
    municipality == "TEXCATEPEC"               ~ 30170,
    municipality == "TEXHUACAN"                ~ 30171,
    municipality == "TEXISTEPEC"               ~ 30172,
    municipality == "TEZONAPA"                 ~ 30173,
    municipality == "TIERRA BLANCA"            ~ 30174,
    municipality == "TIHUATLAN"                ~ 30175,
    municipality == "TLACHICHILCO"             ~ 30180,
    municipality == "TLACOJALPAN"              ~ 30176,
    municipality == "TLACOLULAN"               ~ 30177,
    municipality == "TLACOTALPAN"              ~ 30178,
    municipality == "TLACOTEPEC DE MEJIA"      ~ 30179,
    municipality == "TLALIXCOYAN"              ~ 30181,
    municipality == "TLALNELHUAYOCAN"          ~ 30182,
    municipality == "TLALTETELA"               ~ 30024,
    municipality == "TLAPACOYAN"               ~ 30183,
    municipality == "TLAQUILPAN"               ~ 30184,
    municipality == "TLILAPAN"                 ~ 30185,
    municipality == "TOMATLAN"                 ~ 30186,
    municipality == "TONAYAN"                  ~ 30187,
    municipality == "TOTUTLA"                  ~ 30188,
    municipality == "TRES VALLES"              ~ 30207,
    municipality == "TUXPAN"                   ~ 30189,
    municipality == "TUXTILLA"                 ~ 30190,
    municipality == "URSULO GALVAN"            ~ 30191,
    municipality == "UXPANAPA"                 ~ 30210,
    municipality == "VEGA DE ALATORRE"         ~ 30192,
    municipality == "VERACRUZ"                 ~ 30193,
    municipality == "VILLA ALDAMA"             ~ 30194,
    municipality == "XALAPA"                   ~ 30087,
    municipality == "XICO"                     ~ 30092,
    municipality == "XOXOCOTLA"                ~ 30195,
    municipality == "YANGA"                    ~ 30196,
    municipality == "YECUATLA"                 ~ 30197,
    municipality == "ZACUALPAN"                ~ 30198,
    municipality == "ZARAGOZA"                 ~ 30199,
    municipality == "ZENTLA"                   ~ 30200,
    municipality == "ZONGOLICA"                ~ 30201,
    municipality == "ZONTECOMATLAN"            ~ 30202,
    municipality == "ZOZOCOLCO DE HIDALGO"     ~ 30203,
    TRUE                                       ~ 0
  ))

################################################################################
# 6) gen valid = rowtotal(PAN PRI_PVEM PRD_PT_PC PRV), year=2004, month="September",
#    sort section, and save
################################################################################
df_2004 <- df_merged %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN, PRI_PVEM, PRD_PT_PC, PRV)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2004,
    month = "September"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Lisado Nominal 2010.xlsx" (sheet "Sheet1"), keep certain columns, 
#    drop if missing, convert numeric, sort by `section`, and save "Listanominal2010.dta"
################################################################################

df_listanom <- read_excel(
  path = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Lisado Nominal 2010.xlsx",
  sheet = "Sheet1",
  col_names = TRUE
) %>%
  as.data.frame()
names(df_listanom) <- gsub("[- ]", "", names(df_listanom))
# keep if DISTRITOLOCAL!="" & MUNICIPIO!="" & section!="" & listanominal!=""
# We'll assume the columns are named exactly "DISTRITOLOCAL", "MUNICIPIO", "section", "listanominal"
df_listanom <- df_listanom %>%
  filter(
    DISTRITOLOCAL != "",
    MUNICIPIO      != "",
    section        != "",
    listanominal   != ""
  )

# keep only section, listanominal
df_listanom <- df_listanom %>%
  select(section, listanominal)

# destring => in R: parse as numeric
df_listanom <- df_listanom %>%
  mutate(
    section      = suppressWarnings(as.numeric(section)),
    listanominal = as.numeric(listanominal)
  )

# sort by section
df_listanom <- df_listanom %>%
  arrange(section)

################################################################################
# 2) Read "Ayu_Seccion_2010_No_LN.csv" (Stata: insheet), rename, drop empties, 
#    drop if total==. or 0, parse numeric, collapse, handle coalition columns
################################################################################

df_ayusec <- read_csv("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Ayu_Seccion_2010_No_LN.csv", show_col_types = FALSE) 
colnames(df_ayusec) <- tolower(colnames(df_ayusec))
names(df_ayusec) <- gsub("[- ]", "", names(df_ayusec))
df_ayusec <- df_ayusec %>%
  rename(
    municipality = municipio,
    section      = seccion
  ) %>%
  filter(!(municipality == "" & is.na(section))) %>%
  filter(!(is.na(total) | total == 0))

# parse pan - total as numeric
df_ayusec <- df_ayusec %>%
  mutate(across(pan:total, as.numeric))

# collapse (sum) pan - total by (municipality, section)
df_collapsed <- df_ayusec %>%
  group_by(municipality, section) %>%
  summarise(
    across(pan:total, sum, na.rm=TRUE),
    .groups="drop"
  )

################################################################################
# 3) Create dummy columns (sec_dummy_x) for certain coalitions, then municipality-level flags
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(
    sec_dummy_pan_panal       = if_else(panpanal > 0, 1, 0),
    sec_dummy_pri_pvem_prv    = if_else(pripvemprv > 0, 1, 0),
    sec_dummy_prd_pt_pc       = if_else(prdptpc > 0, 1, 0)
  )

# bys municipality: egen dummy_pan_panal= max(sec_dummy_pan_panal)
# in R, we can do a group_by -> summarise -> left_join approach
df_collapsed_muni <- df_collapsed %>%
  group_by(municipality) %>%
  summarise(
    dummy_pan_panal    = max(sec_dummy_pan_panal, na.rm=TRUE),
    dummy_pri_pvem_prv = max(sec_dummy_pri_pvem_prv, na.rm=TRUE),
    dummy_prd_pt_pc    = max(sec_dummy_prd_pt_pc, na.rm=TRUE),
    .groups="drop"
  )

# "replace dummy_pri_pvem_prv =1 if municipality=='JALCOMULCO'"
df_collapsed_muni <- df_collapsed_muni %>%
  mutate(
    dummy_pri_pvem_prv = if_else(municipality=="JALCOMULCO", 1, dummy_pri_pvem_prv)
  )

# Left join back to df_collapsed
df_collapsed <- df_collapsed %>%
  left_join(df_collapsed_muni, by="municipality") %>%
  select(-sec_dummy_pan_panal, -sec_dummy_pri_pvem_prv, -sec_dummy_prd_pt_pc)

################################################################################
# 4) Create new columns for the coalitions, zero out old columns if coalition used
################################################################################

df_collapsed <- df_collapsed %>%
  mutate(
    pan_panal = if_else(dummy_pan_panal==1, coalesce(pan,0)+coalesce(panal,0)+coalesce(panpanal,0), NA_real_),
    pan = if_else(dummy_pan_panal==1, 0, pan),
    panal = if_else(dummy_pan_panal==1, 0, panal)
  ) %>%
  select(-panpanal)

df_collapsed <- df_collapsed %>%
  mutate(
    pri_pvem_prv = if_else(dummy_pri_pvem_prv==1, coalesce(pripvemprv,0)+coalesce(pri,0)+coalesce(pvem,0)+coalesce(prv,0), NA_real_),
    pri = if_else(dummy_pri_pvem_prv==1, 0, pri),
    pvem= if_else(dummy_pri_pvem_prv==1, 0, pvem),
    prv = if_else(dummy_pri_pvem_prv==1, 0, prv)
  ) %>%
  select(-pripvemprv)

df_collapsed <- df_collapsed %>%
  mutate(
    prd_pt_pc = if_else(dummy_prd_pt_pc==1, coalesce(prdptpc,0)+coalesce(prd,0)+coalesce(pt,0)+coalesce(pc,0), NA_real_),
    prd = if_else(dummy_prd_pt_pc==1, 0, prd),
    pt  = if_else(dummy_prd_pt_pc==1, 0, pt),
    pc  = if_else(dummy_prd_pt_pc==1, 0, pc)
  ) %>%
  select(-prdptpc)

# sum pan - pc pan_panal - prd_pt_pc => just a check in Stata code, we won't replicate "sum" 
# drop dummy_*, pri pvem prv 
df_collapsed <- df_collapsed %>%
  select(-starts_with("dummy_"), -pri, -pvem, -prv)

# rename columns
df_collapsed <- df_collapsed %>%
  rename(
    PAN          = pan,
    PAN_PANAL    = pan_panal,
    PRI_PVEM_PRV = pri_pvem_prv,
    PRD          = prd,
    PRD_PT_PC    = prd_pt_pc,
    PANAL        = panal,
    PT           = pt,
    PC           = pc
  )

################################################################################
# 5) Merge with "Listanominal2010.dta" by 'section', drop unmatched, remove .dta
################################################################################

df_collapsed <- df_collapsed %>%
  arrange(section)

df_merged <- df_collapsed %>%
  left_join(df_listanom, by="section") %>%
  filter(!is.na(listanominal))

################################################################################
# 6) turnout = total / listanominal, drop nulos, noregistrados if exist
################################################################################
df_merged <- df_merged %>%
  mutate(turnout = total / listanominal) %>%
  select(-any_of(c("nulos","noregistrados")))

################################################################################
# 7) Create uniqueid=0, then assign codes for each municipality
################################################################################
df_merged <- df_merged %>%
  mutate(uniqueid=0) %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAJETE"                  ~ 30001,
      municipality == "ACATLAN"                  ~ 30002,
      municipality == "ACAYUCAN"                 ~ 30003,
      municipality == "ACTOPAN"                  ~ 30004,
      municipality == "ACULA"                    ~ 30005,
      municipality == "ACULTZINGO"               ~ 30006,
      municipality == "AGUA DULCE"               ~ 30204,
      municipality == "ALAMO"                    ~ 30160,
      municipality == "ALPATLAHUAC"              ~ 30008,
      municipality == "ALTO LUCERO"              ~ 30009,
      municipality == "ALTOTONGA"                ~ 30010,
      municipality == "ALVARADO"                 ~ 30011,
      municipality == "AMATITLAN"                ~ 30012,
      municipality == "AMATLAN DE LOS REYES"     ~ 30014,
      municipality == "ANGEL R. CABADA"          ~ 30015,
      municipality == "APAZAPAN"                 ~ 30017,
      municipality == "AQUILA"                   ~ 30018,
      municipality == "ASTACINGA"                ~ 30019,
      municipality == "ATLAHUILCO"               ~ 30020,
      municipality == "ATOYAC"                   ~ 30021,
      municipality == "ATZACAN"                  ~ 30022,
      municipality == "ATZALAN"                  ~ 30023,
      municipality == "AYAHUALULCO"              ~ 30025,
      municipality == "BANDERILLA"               ~ 30026,
      municipality == "BENITO JUAREZ"            ~ 30027,
      municipality == "BOCA DEL RIO"             ~ 30028,
      municipality == "CALCAHUALCO"              ~ 30029,
      municipality == "CAMARON DE TEJEDA"        ~ 30007,
      municipality == "CAMERINO Z. MENDOZA"      ~ 30030,
      municipality == "CARLOS A. CARRILLO"       ~ 30208,
      municipality == "CARRILLO PUERTO"          ~ 30031,
      municipality == "CASTILLO DE TEAYO"        ~ 30157,
      municipality == "CATEMACO"                 ~ 30032,
      municipality == "CAZONES DE HERRERA"       ~ 30033,
      municipality == "CERRO AZUL"               ~ 30034,
      municipality == "CHACALTIANGUIS"           ~ 30054,
      municipality == "CHALMA"                   ~ 30055,
      municipality == "CHICONAMEL"               ~ 30056,
      municipality == "CHICONQUIACO"             ~ 30057,
      municipality == "CHICONTEPEC"              ~ 30058,
      municipality == "CHINAMECA"                ~ 30059,
      municipality == "CHINAMPA DE GOROSTIZA"    ~ 30060,
      municipality == "CHOCAMAN"                 ~ 30062,
      municipality == "CHONTLA"                  ~ 30063,
      municipality == "CHUMATLAN"                ~ 30064,
      municipality == "CITLALTEPETL"             ~ 30035,
      municipality == "COACOATZINTLA"            ~ 30036,
      municipality == "COAHUITLAN"               ~ 30037,
      municipality == "COATEPEC"                 ~ 30038,
      municipality == "COATZACOALCOS"            ~ 30039,
      municipality == "COATZINTLA"               ~ 30040,
      municipality == "COETZALA"                 ~ 30041,
      municipality == "COLIPA"                   ~ 30042,
      municipality == "COMAPA"                   ~ 30043,
      municipality == "CORDOBA"                  ~ 30044,
      municipality == "COSAMALOAPAN"             ~ 30045,
      municipality == "COSAUTLAN DE CARVAJAL"    ~ 30046,
      municipality == "COSCOMATEPEC"             ~ 30047,
      municipality == "COSOLEACAQUE"             ~ 30048,
      municipality == "COTAXTLA"                 ~ 30049,
      municipality == "COXQUIHUI"                ~ 30050,
      municipality == "COYUTLA"                  ~ 30051,
      municipality == "CUICHAPA"                 ~ 30052,
      municipality == "CUITLAHUAC"               ~ 30053,
      municipality == "EL HIGO"                  ~ 30205,
      municipality == "EMILIANO ZAPATA"          ~ 30065,
      municipality == "ESPINAL"                  ~ 30066,
      municipality == "FILOMENO MATA"            ~ 30067,
      municipality == "FORTIN"                   ~ 30068,
      municipality == "GUTIERREZ ZAMORA"         ~ 30069,
      municipality == "HIDALGOTITLAN"            ~ 30070,
      municipality == "HUATUSCO"                 ~ 30071,
      municipality == "HUAYACOCOTLA"             ~ 30072,
      municipality == "HUEYAPAN DE OCAMPO"       ~ 30073,
      municipality == "HUILOAPAN DE CUAUHTEMOC"  ~ 30074,
      municipality == "IGNACIO DE LA LLAVE"      ~ 30075,
      municipality == "ILAMATLAN"                ~ 30076,
      municipality == "ISLA"                     ~ 30077,
      municipality == "IXCATEPEC"                ~ 30078,
      municipality == "IXHUACAN DE LOS REYES"    ~ 30079,
      municipality == "IXHUATLAN DE MADERO"      ~ 30083,
      municipality == "IXHUATLAN DEL CAFE"       ~ 30080,
      municipality == "IXHUATLAN DEL SURESTE"    ~ 30082,
      municipality == "IXHUATLANCILLO"           ~ 30081,
      municipality == "IXMATLAHUACAN"            ~ 30084,
      municipality == "IXTACZOQUITLAN"           ~ 30085,
      municipality == "JALACINGO"                ~ 30086,
      municipality == "JALCOMULCO"               ~ 30088,
      municipality == "JALTIPAN"                 ~ 30089,
      municipality == "JAMAPA"                   ~ 30090,
      municipality == "JESUS CARRANZA"           ~ 30091,
      municipality == "JILOTEPEC"                ~ 30093,
      municipality == "JOSE AZUETA"              ~ 30169,
      municipality == "JUAN RODRIGUEZ CLARA"     ~ 30094,
      municipality == "JUCHIQUE DE FERRER"       ~ 30095,
      municipality == "LA ANTIGUA"               ~ 30016,
      municipality == "LA PERLA"                 ~ 30127,
      municipality == "LANDERO Y COSS"           ~ 30096,
      municipality == "LAS CHOAPAS"              ~ 30061,
      municipality == "LAS MINAS"                ~ 30107,
      municipality == "LAS VIGAS DE RAMIREZ"     ~ 30132,
      municipality == "LERDO DE TEJADA"          ~ 30097,
      municipality == "LOS REYES"                ~ 30137,
      municipality == "MAGDALENA"                ~ 30098,
      municipality == "MALTRATA"                 ~ 30099,
      municipality == "MANLIO FABIO ALTAMIRANO"  ~ 30100,
      municipality == "MARIANO ESCOBEDO"         ~ 30101,
      municipality == "MARTINEZ DE LA TORRE"     ~ 30102,
      municipality == "MECATLAN"                 ~ 30103,
      municipality == "MECAYAPAN"                ~ 30104,
      municipality == "MEDELLIN"                 ~ 30105,
      municipality == "MIAHUATLAN"               ~ 30106,
      municipality == "MINATITLAN"               ~ 30108,
      municipality == "MISANTLA"                ~ 30109,
      municipality == "MIXTLA DE ALTAMIRANO"     ~ 30110,
      municipality == "MOLOACAN"                 ~ 30111,
      municipality == "NANCHITAL DE L.C. DEL RIO"~ 30206,
      municipality == "NAOLINCO"                 ~ 30112,
      municipality == "NARANJAL"                 ~ 30113,
      municipality == "NARANJOS AMATLAN"         ~ 30013,
      municipality == "NAUTLA"                   ~ 30114,
      municipality == "NOGALES"                  ~ 30115,
      municipality == "OLUTA"                    ~ 30116,
      municipality == "OMEALCA"                  ~ 30117,
      municipality == "ORIZABA"                  ~ 30118,
      municipality == "OTATITLAN"                ~ 30119,
      municipality == "OTEAPAN"                  ~ 30120,
      municipality == "OZULUAMA"                 ~ 30121,
      municipality == "PAJAPAN"                  ~ 30122,
      municipality == "PANUCO"                   ~ 30123,
      municipality == "PAPANTLA"                 ~ 30124,
      municipality == "PASO DEL MACHO"           ~ 30125,
      municipality == "PASO DE OVEJAS"           ~ 30126,
      municipality == "PEROTE"                   ~ 30128,
      municipality == "PLATON SANCHEZ"           ~ 30129,
      municipality == "PLAYA VICENTE"            ~ 30130,
      municipality == "POZA RICA"                ~ 30131,
      municipality == "PUEBLO VIEJO"             ~ 30133,
      municipality == "PUENTE NACIONAL"          ~ 30134,
      municipality == "RAFAEL DELGADO"           ~ 30135,
      municipality == "RAFAEL LUCIO"             ~ 30136,
      municipality == "RIO BLANCO"               ~ 30138,
      municipality == "SALTABARRANCA"            ~ 30139,
      municipality == "SAN ANDRES TENEJAPAN"     ~ 30140,
      municipality == "SAN ANDRES TUXTLA"        ~ 30141,
      municipality == "SAN JUAN EVANGELISTA"     ~ 30142,
      municipality == "SAN RAFAEL"               ~ 30211,
      municipality == "SANTIAGO SOCHIAPA"        ~ 30212,
      municipality == "SANTIAGO TUXTLA"          ~ 30143,
      municipality == "SAYULA DE ALEMAN"         ~ 30144,
      municipality == "SOCHIAPA"                 ~ 30146,
      municipality == "SOCONUSCO"                ~ 30145,
      municipality == "SOLEDAD ATZOMPA"          ~ 30147,
      municipality == "SOLEDAD DE DOBLADO"       ~ 30148,
      municipality == "SOTEAPAN"                 ~ 30149,
      municipality == "TAMALIN"                  ~ 30150,
      municipality == "TAMIAHUA"                 ~ 30151,
      municipality == "TAMPICO ALTO"             ~ 30152,
      municipality == "TANCOCO"                  ~ 30153,
      municipality == "TANTIMA"                  ~ 30154,
      municipality == "TANTOYUCA"                ~ 30155,
      municipality == "TATAHUICAPAN"             ~ 30209,
      municipality == "TATATILA"                 ~ 30156,
      municipality == "TECOLUTLA"                ~ 30158,
      municipality == "TEHUIPANGO"               ~ 30159,
      municipality == "TEMPOAL"                  ~ 30161,
      municipality == "TENAMPA"                  ~ 30162,
      municipality == "TENOCHTITLAN"             ~ 30163,
      municipality == "TEOCELO"                  ~ 30164,
      municipality == "TEPATLAXCO"               ~ 30165,
      municipality == "TEPETLAN"                 ~ 30166,
      municipality == "TEPETZINTLA"              ~ 30167,
      municipality == "TEQUILA"                  ~ 30168,
      municipality == "TEXCATEPEC"               ~ 30170,
      municipality == "TEXHUACAN"                ~ 30171,
      municipality == "TEXISTEPEC"               ~ 30172,
      municipality == "TEZONAPA"                 ~ 30173,
      municipality == "TIERRA BLANCA"            ~ 30174,
      municipality == "TIHUATLAN"                ~ 30175,
      municipality == "TLACHICHILCO"             ~ 30180,
      municipality == "TLACOJALPAN"              ~ 30176,
      municipality == "TLACOLULAN"               ~ 30177,
      municipality == "TLACOTALPAN"              ~ 30178,
      municipality == "TLACOTEPEC DE MEJIA"      ~ 30179,
      municipality == "TLALIXCOYAN"              ~ 30181,
      municipality == "TLALNELHUAYOCAN"          ~ 30182,
      municipality == "TLALTETELA"               ~ 30024,
      municipality == "TLAPACOYAN"               ~ 30183,
      municipality == "TLAQUILPA"                ~ 30184,
      municipality == "TLILAPAN"                 ~ 30185,
      municipality == "TOMATLAN"                 ~ 30186,
      municipality == "TONAYAN"                  ~ 30187,
      municipality == "TOTUTLA"                  ~ 30188,
      municipality == "TRES VALLES"              ~ 30207,
      municipality == "TUXPAN"                   ~ 30189,
      municipality == "TUXTILLA"                 ~ 30190,
      municipality == "URSULO GALVAN"            ~ 30191,
      municipality == "UXPANAPA"                 ~ 30210,
      municipality == "VEGA DE ALATORRE"         ~ 30192,
      municipality == "VERACRUZ"                 ~ 30193,
      municipality == "VILLA ALDAMA"             ~ 30194,
      municipality == "XALAPA"                   ~ 30087,
      municipality == "XICO"                     ~ 30092,
      municipality == "XOXOCOTLA"                ~ 30195,
      municipality == "YANGA"                    ~ 30196,
      municipality == "YECUATLA"                 ~ 30197,
      municipality == "ZACUALPAN"                ~ 30198,
      municipality == "ZARAGOZA"                 ~ 30199,
      municipality == "ZENTLA"                   ~ 30200,
      municipality == "ZONGOLICA"                ~ 30201,
      municipality == "ZONTECOMATLAN"            ~ 30202,
      municipality == "ZOZOCOLCO DE HIDALGO"     ~ 30203,
      TRUE                                       ~ 0
    )
  )

################################################################################
# 8) Create valid = rowtotal(PAN PANAL PRD PT PC PAN_PANAL PRI_PVEM_PRV PRD_PT_PC),
#    year=2010, month="July", sort by section, save "Veracruz_Section_2010.dta"
################################################################################
df_2010 <- df_merged %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(c(
      "PAN","PANAL","PRD","PT","PC","PAN_PANAL","PRI_PVEM_PRV","PRD_PT_PC"
    ))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year  = 2010,
    month = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Read "Ayu_Seccion_2013_No_LN.dta" into R
#    (Equivalent to: use Ayu_Seccion_2013_No_LN.dta, clear)
################################################################################
df_main <- read_dta("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/Ayu_Seccion_2013_No_LN.dta")

################################################################################
# 2) Rename columns: MC -> PC, PFCRN -> PartCardenista
#    Then drop NoRegistrados, Nulos (columns, presumably)
################################################################################
df_main <- df_main %>%
  rename(
    PC             = MC,
    PartCardenista = PFCRN
  ) %>%
  select(-any_of(c("NoRegistrados","Nulos")))

################################################################################
# 3) Create uniqueid=0, then assign municipality codes
################################################################################
df_main <- df_main %>%
  mutate(uniqueid = 0) %>%
  mutate(
    uniqueid = case_when(
      municipality == "ACAJETE"                ~ 30001,
      municipality == "ACATLAN"               ~ 30002,
      municipality == "ACAYUCAN"              ~ 30003,
      municipality == "ACTOPAN"               ~ 30004,
      municipality == "ACULA"                 ~ 30005,
      municipality == "ACULTZINGO"            ~ 30006,
      municipality == "AGUA DULCE"            ~ 30204,
      municipality == "ALAMO TEMAPACHE"       ~ 30160,
      municipality == "ALPATLAHUAC"           ~ 30008,
      municipality == "ALTO LUCERO"           ~ 30009,
      municipality == "ALTOTONGA"             ~ 30010,
      municipality == "ALVARADO"              ~ 30011,
      municipality == "AMATITLAN"             ~ 30012,
      municipality == "AMATLAN DE LOS REYES"  ~ 30014,
      municipality == "ANGEL R. CABADA"       ~ 30015,
      municipality == "APAZAPAN"              ~ 30017,
      municipality == "AQUILA"                ~ 30018,
      municipality == "ASTACINGA"             ~ 30019,
      municipality == "ATLAHUILCO"            ~ 30020,
      municipality == "ATOYAC"                ~ 30021,
      municipality == "ATZACAN"               ~ 30022,
      municipality == "ATZALAN"               ~ 30023,
      municipality == "AYAHUALULCO"           ~ 30025,
      municipality == "BANDERILLA"            ~ 30026,
      municipality == "BENITO JUAREZ"         ~ 30027,
      municipality == "BOCA DEL RIO"          ~ 30028,
      municipality == "CALCAHUALCO"           ~ 30029,
      municipality == "CAMARON DE TEJEDA"     ~ 30007,
      municipality == "CAMERINO Z. MENDOZA"   ~ 30030,
      municipality == "CARLOS A. CARRILLO"    ~ 30208,
      municipality == "CARRILLO PUERTO"       ~ 30031,
      municipality == "CASTILLO DE TEAYO"     ~ 30157,
      municipality == "CATEMACO"              ~ 30032,
      municipality == "CAZONES DE HERRERA"    ~ 30033,
      municipality == "CERRO AZUL"            ~ 30034,
      municipality == "CHACALTIANGUIS"        ~ 30054,
      municipality == "CHALMA"                ~ 30055,
      municipality == "CHICONAMEL"            ~ 30056,
      municipality == "CHICONQUIACO"          ~ 30057,
      municipality == "CHICONTEPEC"           ~ 30058,
      municipality == "CHINAMECA"             ~ 30059,
      municipality == "CHINAMPA DE GOROSTIZA" ~ 30060,
      municipality == "CHOCAMAN"              ~ 30062,
      municipality == "CHONTLA"               ~ 30063,
      municipality == "CHUMATLAN"             ~ 30064,
      municipality == "CITLALTEPEC"           ~ 30035,
      municipality == "COACOATZINTLA"         ~ 30036,
      municipality == "COAHUITLAN"            ~ 30037,
      municipality == "COATEPEC"              ~ 30038,
      municipality == "COATZACOALCOS"         ~ 30039,
      municipality == "COATZINTLA"            ~ 30040,
      municipality == "COETZALA"              ~ 30041,
      municipality == "COLIPA"                ~ 30042,
      municipality == "COMAPA"                ~ 30043,
      municipality == "CORDOBA"               ~ 30044,
      municipality == "COSAMALOAPAN"          ~ 30045,
      municipality == "COSAUTLAN DE CARBAJAL" ~ 30046,
      municipality == "COSCOMATEPEC"          ~ 30047,
      municipality == "COSOLEACAQUE"          ~ 30048,
      municipality == "COTAXTLA"              ~ 30049,
      municipality == "COXQUIHUI"             ~ 30050,
      municipality == "COYUTLA"               ~ 30051,
      municipality == "CUICHAPA"              ~ 30052,
      municipality == "CUITLAHUAC"            ~ 30053,
      municipality == "EL HIGO"               ~ 30205,
      municipality == "EMILIANO ZAPATA"       ~ 30065,
      municipality == "ESPINAL"               ~ 30066,
      municipality == "FILOMENO MATA"         ~ 30067,
      municipality == "FORTIN"                ~ 30068,
      municipality == "GUTIERREZ ZAMORA"      ~ 30069,
      municipality == "HIDALGOTITLAN"         ~ 30070,
      municipality == "HUATUSCO"              ~ 30071,
      municipality == "HUAYACOCOTLA"          ~ 30072,
      municipality == "HUEYAPAN DE OCAMPO"    ~ 30073,
      municipality == "HUILOAPAN"             ~ 30074,
      municipality == "IGNACIO DE LA LLAVE"   ~ 30075,
      municipality == "ILAMATLAN"             ~ 30076,
      municipality == "ISLA"                  ~ 30077,
      municipality == "IXCATEPEC"             ~ 30078,
      municipality == "IXHUACAN DE LOS REYES" ~ 30079,
      municipality == "IXHUATLAN DE MADERO"   ~ 30083,
      str_detect(municipality, "IXHUATLAN DEL CAF") ~ 30080,
      municipality == "IXHUATLAN DEL SURESTE" ~ 30082,
      municipality == "IXHUATLANCILLO"        ~ 30081,
      municipality == "IXMATLAHUACAN"         ~ 30084,
      municipality == "IXTACZOQUITLAN"        ~ 30085,
      municipality == "JALACINGO"             ~ 30086,
      municipality == "JALCOMULCO"            ~ 30088,
      municipality == "JALTIPAN"              ~ 30089,
      municipality == "JAMAPA"                ~ 30090,
      municipality == "JESUS CARRANZA"        ~ 30091,
      municipality == "JILOTEPEC"             ~ 30093,
      municipality == "JOSE AZUETA"           ~ 30169,
      municipality == "JUAN RODRIGUEZ CLARA"  ~ 30094,
      municipality == "JUCHIQUE DE FERRER"    ~ 30095,
      municipality == "LA ANTIGUA"            ~ 30016,
      municipality == "LA PERLA"              ~ 30127,
      municipality == "LANDERO Y COSS"        ~ 30096,
      municipality == "LAS CHOAPAS"           ~ 30061,
      municipality == "LAS MINAS"             ~ 30107,
      municipality == "LAS VIGAS"             ~ 30132,
      municipality == "LERDO DE TEJADA"       ~ 30097,
      municipality == "LOS REYES"             ~ 30137,
      municipality == "MAGDALENA"             ~ 30098,
      municipality == "MALTRATA"              ~ 30099,
      municipality == "MANLIO FABIO ALTAMIRANO" ~ 30100,
      municipality == "MARIANO ESCOBEDO"      ~ 30101,
      municipality == "MARTINEZ DE LA TORRE"  ~ 30102,
      municipality == "MECATLAN"              ~ 30103,
      municipality == "MECAYAPAN"             ~ 30104,
      municipality == "MEDELLIN"             ~ 30105,
      municipality == "MIAHUATLAN"            ~ 30106,
      municipality == "MINATITLAN"            ~ 30108,
      municipality == "MISANTLA"             ~ 30109,
      municipality == "MIXTLA DE ALTAMIRANO"  ~ 30110,
      municipality == "MOLOACAN"              ~ 30111,
      municipality == "NANCHITAL DE L. C. DEL R." ~ 30206,
      municipality == "NAOLINCO"              ~ 30112,
      municipality == "NARANJAL"              ~ 30113,
      municipality == "NARANJOS AMATLAN"      ~ 30013,
      municipality == "NAUTLA"                ~ 30114,
      municipality == "NOGALES"               ~ 30115,
      municipality == "OLUTA"                 ~ 30116,
      municipality == "OMEALCA"               ~ 30117,
      municipality == "ORIZABA"               ~ 30118,
      municipality == "OTATITLAN"             ~ 30119,
      municipality == "OTEAPAN"               ~ 30120,
      municipality == "OZULUAMA"              ~ 30121,
      municipality == "PAJAPAN"               ~ 30122,
      municipality == "PANUCO"                ~ 30123,
      municipality == "PAPANTLA"              ~ 30124,
      municipality == "PASO DE OVEJAS"        ~ 30126,
      municipality == "PASO DEL MACHO"        ~ 30125,
      municipality == "PEROTE"                ~ 30128,
      municipality == "PLATON SANCHEZ"        ~ 30129,
      municipality == "PLAYA VICENTE"         ~ 30130,
      municipality == "POZA RICA"             ~ 30131,
      municipality == "PUEBLO VIEJO"          ~ 30133,
      municipality == "PUENTE NACIONAL"       ~ 30134,
      municipality == "RAFAEL DELGADO"        ~ 30135,
      municipality == "RAFAEL LUCIO"          ~ 30136,
      municipality == "RIO BLANCO"            ~ 30138,
      municipality == "SALTABARRANCA"         ~ 30139,
      municipality == "SAN ANDRES TENEJAPAN"  ~ 30140,
      municipality == "SAN ANDRES TUXTLA"     ~ 30141,
      municipality == "SAN JUAN EVANGELISTA"  ~ 30142,
      municipality == "SAN RAFAEL"            ~ 30211,
      municipality == "SANTIAGO SOCHIAPAN"    ~ 30212,
      municipality == "SANTIAGO TUXTLA"       ~ 30143,
      municipality == "SAYULA DE ALEMAN"      ~ 30144,
      municipality == "SOCHIAPA"              ~ 30146,
      municipality == "SOCONUSCO"             ~ 30145,
      municipality == "SOLEDAD ATZOMPA"       ~ 30147,
      municipality == "SOLEDAD DE DOBLADO"    ~ 30148,
      municipality == "SOTEAPAN"              ~ 30149,
      municipality == "TAMALIN"               ~ 30150,
      municipality == "TAMIAHUA"              ~ 30151,
      municipality == "TAMPICO ALTO"          ~ 30152,
      municipality == "TANCOCO"               ~ 30153,
      municipality == "TANTIMA"               ~ 30154,
      municipality == "TANTOYUCA"             ~ 30155,
      municipality == "TATAHUICAPAN"          ~ 30209,
      municipality == "TATATILA"              ~ 30156,
      municipality == "TECOLUTLA"             ~ 30158,
      municipality == "TEHUIPANGO"            ~ 30159,
      municipality == "TEMPOAL"               ~ 30161,
      municipality == "TENAMPA"               ~ 30162,
      municipality == "TENOCHTITLAN"          ~ 30163,
      municipality == "TEOCELO"               ~ 30164,
      municipality == "TEPATLAXCO"            ~ 30165,
      municipality == "TEPETLAN"              ~ 30166,
      municipality == "TEPETZINTLA"           ~ 30167,
      municipality == "TEQUILA"               ~ 30168,
      municipality == "TEXCATEPEC"            ~ 30170,
      municipality == "TEXHUACAN"             ~ 30171,
      municipality == "TEXISTEPEC"            ~ 30172,
      municipality == "TEZONAPA"              ~ 30173,
      municipality == "TIERRA BLANCA"         ~ 30174,
      municipality == "TIHUATLAN"             ~ 30175,
      municipality == "TLACHICHILCO"          ~ 30180,
      municipality == "TLACOJALPAN"           ~ 30176,
      municipality == "TLACOLULAN"            ~ 30177,
      municipality == "TLACOTALPAN"           ~ 30178,
      municipality == "TLACOTEPEC DE MEJIA"   ~ 30179,
      municipality == "TLALIXCOYAN"           ~ 30181,
      municipality == "TLALNELHUAYOCAN"       ~ 30182,
      municipality == "TLALTETELA"            ~ 30024,
      municipality == "TLAPACOYAN"            ~ 30183,
      municipality == "TLAQUILPA"             ~ 30184,
      municipality == "TLILAPAN"              ~ 30185,
      municipality == "TOMATLAN"              ~ 30186,
      municipality == "TONAYAN"               ~ 30187,
      municipality == "TOTUTLA"               ~ 30188,
      municipality == "TRES VALLES"           ~ 30207,
      municipality == "TUXPAN"                ~ 30189,
      municipality == "TUXTILLA"              ~ 30190,
      municipality == "URSULO GALVAN"         ~ 30191,
      municipality == "UXPANAPA"              ~ 30210,
      municipality == "VEGA DE ALATORRE"      ~ 30192,
      municipality == "VERACRUZ"              ~ 30193,
      municipality == "VILLA ALDAMA"          ~ 30194,
      municipality == "XALAPA"                ~ 30087,
      municipality == "XICO"                  ~ 30092,
      municipality == "XOXOCOTLA"             ~ 30195,
      municipality == "YANGA"                 ~ 30196,
      municipality == "YECUATLA"              ~ 30197,
      municipality == "ZACUALPAN"             ~ 30198,
      municipality == "ZARAGOZA"              ~ 30199,
      municipality == "ZENTLA"                ~ 30200,
      municipality == "ZONGOLICA"             ~ 30201,
      municipality == "ZONTECOMATLAN"         ~ 30202,
      municipality == "ZOZOCOLCO"             ~ 30203,
      TRUE                                    ~ 0
    )
  )

################################################################################
# 4) Summaries by uniqueid (bys uniqueid: egen mun_var= sum(var), etc.), 
#    then merge with "all_months_years.dta" for (ed=30, month=6, year=2013).
################################################################################

coal_cols <- c("PAN","PRI_PVEM_PANAL","PRD","PT","PC","AVE","PartCardenista","total","valid")

# We'll create sums by uniqueid, then create inverse.
for (var_ in coal_cols) {
  if (var_ %in% names(df_main)) {
    # sum by uniqueid
    df_sums <- df_main %>%
      group_by(uniqueid) %>%
      summarise(mun_val = sum(.data[[var_]], na.rm = TRUE), .groups="drop")
    # We'll left_join back to df_main
    df_main <- df_main %>%
      left_join(df_sums, by="uniqueid")
    # rename columns
    new_mun_name  <- paste0("mun_", var_)
    new_inv_name  <- paste0("inv_mun_", var_)
    df_main <- df_main %>%
      rename(!! new_mun_name := mun_val) %>%
      mutate(
        !! new_inv_name := 1 / .data[[new_mun_name]]
      )
  }
}

# Now set ed=30, seccion=section
df_main <- df_main %>%
  mutate(
    ed      = 30,
    seccion = section
  )

# Merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
df_all <- read_dta("../../all_months_years.dta")

df_all_sub <- df_all %>%
  select(ed, seccion, month, year, lista) %>%
  filter(month==6, year==2013)


# We'll do left_join => in Stata code it's 1:m. We'll replicate as best as possible:
df_merged <- df_main %>%
  left_join(df_all_sub, by=c("ed","seccion"))

# drop if _merge==2 => in R means dropping rows with no match => check if 'lista' is NA
df_merged <- df_merged %>%
  filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
df_merged <- df_merged %>%
  select(-any_of(c("ed","seccion","year","month")))

# rename lista->listanominal
df_merged <- df_merged %>%
  rename(listanominal = lista)

# gen turnout= total/listanominal, year=2013, month="July"
df_2013 <- df_merged %>%
  mutate(
    turnout = total / listanominal,
    year    = 2013,
    month   = "July"
  ) %>%
  arrange(section)

################################################################################
# 1) Reading "Ayuntamientos_2016.xlsx" sheets, converting to .dta
################################################################################

# In Stata:
# import excel "Ayuntamientos_2016.xlsx", describe
# forvalues sheet=1/`=r(N_worksheet)' { ... }

# In R, we replicate that logic by discovering all sheets in "Ayuntamientos_2016.xlsx"
all_sheets <- readxl::excel_sheets("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/Ayuntamientos_2016.xlsx")

for (sheetname in all_sheets) {
  # Read the sheet, all columns as strings
  df_sheet <- read_excel(
    path    = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/Ayuntamientos_2016.xlsx",
    sheet   = sheetname,
    col_names = TRUE,
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # Sanitize column names
  df_sheet <- df_sheet %>%
    janitor::clean_names() %>%
    rename_with(~ str_trunc(., 32, ellipsis = "")) # Truncate to 32 characters
  
  # Replace municipality with its first value
  if (nrow(df_sheet) > 0 && "municipality" %in% names(df_sheet)) {
    df_sheet$municipality <- df_sheet$municipality[1]
  }
  
  # Drop rows where Seccion == "TOTAL" or Seccion == ""
  if ("seccion" %in% names(df_sheet)) {
    df_sheet <- df_sheet %>%
      filter(!(seccion %in% c("TOTAL", "")))
  }
  
  # Replace empty strings with "0"
  df_sheet <- df_sheet %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # Save as `sheetname`.dta
  write_dta(df_sheet, paste0(sheetname, ".dta"))
}


################################################################################
# 2) Clear and append multiple .dta files: "Resultados.dta", "Resultados (2).dta", ...
################################################################################

# We'll assume you have a known list of .dta files or you collect them from directory
append_files <- c(
  "Resultados.dta", "Resultados (2).dta", "Resultados (3).dta", "Resultados (4).dta",
  "Resultados (5).dta", "Resultados (6).dta", "Resultados (7).dta", "Resultados (8).dta",
  "Resultados (10).dta", "Resultados (11).dta", "Resultados (12).dta", "Resultados (13).dta",
  "Resultados (14).dta", "Resultados (15).dta", "Resultados (16).dta", "Resultados (17).dta",
  "Resultados (18).dta", "Resultados (19).dta", "Resultados (20).dta", "Resultados (21).dta",
  "Resultados (22).dta", "Resultados (23).dta", "Resultados (24).dta", "Resultados (25).dta",
  "Resultados (26).dta", "Resultados (27).dta", "Resultados (28).dta", "Resultados (29).dta",
  "Resultados (30).dta", "Resultados (31).dta", "Resultados (32).dta", "Resultados (33).dta",
  "Resultados (34).dta", "Resultados (35).dta", "Resultados (36).dta", "Resultados (37).dta",
  "Resultados (38).dta", "Resultados (39).dta", "Resultados (40).dta", "Resultados (41).dta",
  "Resultados (42).dta", "Resultados (43).dta", "Resultados (44).dta", "Resultados (45).dta",
  "Resultados (46).dta", "Resultados (47).dta", "Resultados (48).dta", "Resultados (49).dta",
  "Resultados (50).dta", "Resultados (51).dta", "Resultados (52).dta", "Resultados (53).dta",
  "Resultados (54).dta", "Resultados (55).dta", "Resultados (56).dta", "Resultados (57).dta",
  "Resultados (58).dta", "Resultados (59).dta", "Resultados (60).dta", "Resultados (61).dta",
  "Resultados (62).dta", "Resultados (63).dta", "Resultados (64).dta", "Resultados (65).dta",
  "Resultados (66).dta", "Resultados (67).dta", "Resultados (68).dta", "Resultados (69).dta",
  "Resultados (70).dta", "Resultados (71).dta", "Resultados (72).dta", "Resultados (73).dta",
  "Resultados (74).dta", "Resultados (75).dta", "Resultados (76).dta", "Resultados (77).dta",
  "Resultados (78).dta", "Resultados (79).dta", "Resultados (80).dta", "Resultados (81).dta",
  "Resultados (82).dta", "Resultados (83).dta", "Resultados (84).dta", "Resultados (85).dta",
  "Resultados (86).dta", "Resultados (87).dta", "Resultados (88).dta", "Resultados (89).dta",
  "Resultados (90).dta", "Resultados (91).dta", "Resultados (92).dta", "Resultados (93).dta",
  "Resultados (94).dta", "Resultados (95).dta", "Resultados (96).dta", "Resultados (97).dta",
  "Resultados (98).dta", "Resultados (99).dta", "Resultados (100).dta", "Resultados (101).dta",
  "Resultados (102).dta", "Resultados (103).dta", "Resultados (104).dta", "Resultados (105).dta",
  "Resultados (106).dta", "Resultados (107).dta", "Resultados (108).dta", "Resultados (109).dta",
  "Resultados (110).dta", "Resultados (111).dta", "Resultados (112).dta", "Resultados (113).dta",
  "Resultados (114).dta", "Resultados (115).dta", "Resultados (116).dta", "Resultados (117).dta",
  "Resultados (118).dta", "Resultados (119).dta", "Resultados (120).dta", "Resultados (121).dta",
  "Resultados (122).dta", "Resultados (123).dta", "Resultados (124).dta", "Resultados (125).dta",
  "Resultados (126).dta", "Resultados (127).dta", "Resultados (128).dta", "Resultados (129).dta",
  "Resultados (130).dta", "Resultados (131).dta", "Resultados (132).dta", "Resultados (133).dta",
  "Resultados (134).dta", "Resultados (135).dta", "Resultados (136).dta", "Resultados (137).dta",
  "Resultados (138).dta", "Resultados (139).dta", "Resultados (140).dta", "Resultados (141).dta",
  "Resultados (142).dta", "Resultados (143).dta", "Resultados (144).dta", "Resultados (145).dta",
  "Resultados (146).dta", "Resultados (147).dta", "Resultados (148).dta", "Resultados (149).dta",
  "Resultados (150).dta", "Resultados (151).dta", "Resultados (152).dta", "Resultados (153).dta",
  "Resultados (154).dta", "Resultados (155).dta", "Resultados (156).dta", "Resultados (157).dta",
  "Resultados (158).dta", "Resultados (159).dta", "Resultados (160).dta", "Resultados (161).dta",
  "Resultados (162).dta", "Resultados (164).dta", "Resultados (165).dta", "Resultados (166).dta",
  "Resultados (167).dta", "Resultados (168).dta", "Resultados (169).dta", "Resultados (171).dta",
  "Resultados (172).dta", "Resultados (173).dta", "Resultados (174).dta", "Resultados (175).dta",
  "Resultados (176).dta", "Resultados (177).dta", "Resultados (178).dta"
)

df_all <- data.frame()

for (f in append_files) {
  if (file.exists(f)) {
    df_new <- read_dta(f)
    df_all <- bind_rows(df_all, df_new)
  }
}

# gen municipio=subinstr(municipality,"Reporte de Casillas del municipio de ","",1)
df_all <- df_all %>%
  mutate(
    municipio = str_replace(municipality, 
                            "Reporte de Casillas del municipio de ", "")
  )

################################################################################
# 3) Another set: "AyuntamientosB" folder, "Ayuntamientos_2016b.xlsx", reading multiple sheets
################################################################################

all_sheets_b <- readxl::excel_sheets("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/AyuntamientosB/Ayuntamientos_2016b.xlsx")

for (sheetname in all_sheets_b) {
  # Read the sheet, all columns as strings
  df_sheet_b <- read_excel(
    path = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/AyuntamientosB/Ayuntamientos_2016b.xlsx",
    sheet = sheetname,
    col_names = TRUE,
    col_types = "text"
  ) %>%
    as.data.frame()
  
  # Sanitize column names
  df_sheet_b <- df_sheet_b %>%
    janitor::clean_names() %>%
    rename_with(~ str_trunc(., 32, ellipsis = "")) # Truncate to 32 characters
  
  # Replace municipality with its first value
  if (nrow(df_sheet_b) > 0 && "municipality" %in% names(df_sheet_b)) {
    df_sheet_b$municipality <- df_sheet_b$municipality[1]
  }
  
  # Drop rows where Seccion == "TOTAL" or Seccion == ""
  if ("seccion" %in% names(df_sheet_b)) {
    df_sheet_b <- df_sheet_b %>%
      filter(!(seccion %in% c("TOTAL", "")))
  }
  
  # Replace empty strings with "0"
  df_sheet_b <- df_sheet_b %>%
    mutate(across(everything(), ~ if_else(. == "", "0", .)))
  
  # Save as `sheetname`.dta
  write_dta(df_sheet_b, paste0(sheetname, ".dta"))
}


# clear, then appends: "Resultados.dta", "Resultados (2).dta", ...
append_files_b <- c("Resultados.dta","Resultados (2).dta","Resultados (3).dta","Resultados (4).dta","Resultados (5).dta",
                    "Resultados (6).dta","Resultados (7).dta","Resultados (8).dta","Resultados (9).dta","Resultados (10).dta",
                    # etc. fill in as needed
                    "Resultados (16).dta","Resultados (17).dta","Resultados (18).dta","Resultados (19).dta",
                    "Resultados (20).dta","Resultados (21).dta","Resultados (22).dta","Resultados (23).dta","Resultados (24).dta",
                    "Resultados (25).dta","Resultados (26).dta","Resultados (27).dta","Resultados (28).dta","Resultados (29).dta",
                    "Resultados (30).dta","Resultados (31).dta","Resultados (32).dta","Resultados (33).dta","Resultados (34).dta",
                    "Resultados (35).dta","Resultados (36).dta")

df_b_all <- data.frame()
for (f in append_files_b) {
  if (file.exists(f)) {
    df_new_b <- read_dta(f)
    df_b_all <- bind_rows(df_b_all, df_new_b)
  }
}

# gen municipio=subinstr(municipality,"Reporte de Casillas del municipio de ","",1)
df_b_all <- df_b_all %>%
  mutate(
    municipio = str_replace(municipality, 
                            "Reporte de Casillas del municipio de ", "")
  )

# drop if municipio=="Jaltipan" or "Zontecomatlán"
df_b_all <- df_b_all %>%
  filter(!(municipio %in% c("Jaltipan","Zontecomatlán")))

################################################################################
# 4) Now returning to parent directory, read "Veracruz_Section_2017.dta", append "Veracruz_Section_2016b.dta"
################################################################################

df_ver1 <- read_dta("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/AyuntamientosB/Veracruz_Section_2017b.dta")
df_ver2 <- read_dta("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/Other/AyuntamientosB/Veracruz_Section_2016b.dta")

df_ver <- bind_rows(df_ver1, df_ver2)

# destring * => in R, parse columns as numeric if needed:
df_ver <- df_ver %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))

# drop O P if exist
df_ver <- df_ver %>%
  select(-any_of(c("O","P")))

# drop municipality => rename municipio->municipality
# rename Seccion->section
if ("municipality" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    select(-municipality)
}
if ("Seccion" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    rename(section = Seccion)
}
df_ver <- df_ver %>%
  rename(municipality = municipio)

# reorder
df_ver <- df_ver %>%
  select(municipality, section, everything())

################################################################################
# 5) Coalition handling: 
#    replace CoalicionPANPRD=CoalicionPANPRD + PAN + PRD if CoalicionPANPRD!=. ...
#    and similarly for others.
################################################################################

# We'll replicate the logic in R with if_else or something similar.
# Because the code is quite repetitive, we do carefully:

if ("CoalicionPANPRD" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    mutate(
      CoalicionPANPRD = if_else(!is.na(CoalicionPANPRD),
                                CoalicionPANPRD + coalesce(PAN,0) + coalesce(PRD,0),
                                CoalicionPANPRD),
      PAN = if_else(!is.na(CoalicionPANPRD), NA_real_, PAN),
      PRD = if_else(!is.na(CoalicionPANPRD), NA_real_, PRD)
    )
}

# Another line: replace CoaliciónPANPRD=CoaliciónPANPRD + PAN + PRD
if ("CoaliciónPANPRD" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    mutate(
      CoaliciónPANPRD = if_else(!is.na(CoaliciónPANPRD),
                                CoaliciónPANPRD + coalesce(PAN,0) + coalesce(PRD,0),
                                CoaliciónPANPRD),
      PAN = if_else(!is.na(CoaliciónPANPRD), NA_real_, PAN),
      PRD = if_else(!is.na(CoaliciónPANPRD), NA_real_, PRD)
    )
}

if ("VERDE" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    rename(PVEM = VERDE)
}

if ("CoalicionPRIVERDE" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    mutate(
      CoalicionPRIVERDE = if_else(!is.na(CoalicionPRIVERDE),
                                  CoalicionPRIVERDE + coalesce(PRI,0) + coalesce(PVEM,0),
                                  CoalicionPRIVERDE),
      PRI  = if_else(!is.na(CoalicionPRIVERDE), NA_real_, PRI),
      PVEM = if_else(!is.na(CoalicionPRIVERDE), NA_real_, PVEM)
    )
}

if ("CoalicionPriverde" %in% names(df_ver)) {
  df_ver <- df_ver %>%
    mutate(
      CoalicionPriverde = if_else(!is.na(CoalicionPriverde),
                                  CoalicionPriverde + coalesce(PRI,0) + coalesce(PVEM,0),
                                  CoalicionPriverde),
      PRI  = if_else(!is.na(CoalicionPriverde), NA_real_, PRI),
      PVEM = if_else(!is.na(CoalicionPriverde), NA_real_, PVEM)
    )
}

# collapse (sum) columns by (municipality, section)
df_collapsed <- df_ver %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~ sum(. , na.rm=TRUE)), .groups="drop")

# gen nulo=Q + VotosNulos
df_collapsed <- df_collapsed %>%
  mutate(
    nulo = coalesce(Q,0) + coalesce(VotosNulos,0)
  ) %>%
  select(-Q, -VotosNulos)

# gen PRI_PVEM=CoalicionPRIVERDE+CoalicionPriverde
df_collapsed <- df_collapsed %>%
  mutate(
    PRI_PVEM = coalesce(CoalicionPRIVERDE,0) + coalesce(CoalicionPriverde,0)
  ) %>%
  select(-CoalicionPRIVERDE, -CoalicionPriverde)

# gen PANAL= NuevaAlianza + NuevaALianza + Nuevaalianza
df_collapsed <- df_collapsed %>%
  mutate(
    PANAL = rowSums(across(c("NuevaAlianza","NuevaALianza","Nuevaalianza"), 
                           ~ as.numeric(.)), na.rm=TRUE)
  ) %>%
  select(-NuevaAlianza, -NuevaALianza, -Nuevaalianza)

# gen MC= MovimientoCiudadano + MovimientoCuidadano
df_collapsed <- df_collapsed %>%
  mutate(
    MC = rowSums(across(c("MovimientoCiudadano","MovimientoCuidadano"), 
                        ~ as.numeric(.)), na.rm=TRUE)
  ) %>%
  select(-MovimientoCiudadano, -MovimientoCuidadano)

# replace PT= PT + Pt + pt
pt_cols <- c("PT","Pt","pt")
pt_sum <- rowSums(df_collapsed[pt_cols], na.rm=TRUE)
df_collapsed <- df_collapsed %>%
  mutate(
    PT = pt_sum
  ) %>%
  select(-any_of(c("Pt","pt")))

# gen PAN_PRD=CoaliciónPANPRD + CoalicionPANPRD
df_collapsed <- df_collapsed %>%
  mutate(
    PAN_PRD = coalesce(CoaliciónPANPRD,0) + coalesce(CoalicionPANPRD,0)
  ) %>%
  select(-CoaliciónPANPRD, -CoalicionPANPRD)

# rename (EncuentroSocial CNR) -> (PES no_reg)
df_collapsed <- df_collapsed %>%
  rename(
    PES    = EncuentroSocial,
    no_reg = CNR
  )

# create CI_1, CI_2, CI_3 from row sums across specified columns, then manipulate them
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    CI_1 = sum(c_across(ChistianRomeroPerez:MayrethMartínezPeñaloza), na.rm=TRUE),
    CI_2 = sum(c_across(c("EduardoCidJuarez","MauricioIvanAguilleMarín","VíctorManuelMurrietaPérez",
                          "MartínGarcíaMartínez","MiguelÁngelMartínezDionisio","OscarOctacioGreerBecerra",
                          "EmilioÁlvarezPimentel","AntonioLunaAndrade","RafaelVegaHernández","MayrethMartínezPeñaloza")),
               na.rm=TRUE),
    CI_3 = sum(c_across(c("ChristopherCristianCházaro","RubénMorenoArcher")), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    CI_1 = CI_1 - CI_2 - CI_3
  ) %>%
  select(-ChistianRomeroPerez, -MayrethMartínezPeñaloza, -one_of(c("EduardoCidJuarez","MauricioIvanAguilleMarín",
                                                                   "VíctorManuelMurrietaPérez","MartínGarcíaMartínez","MiguelÁngelMartínezDionisio","OscarOctacioGreerBecerra",
                                                                   "EmilioÁlvarezPimentel","AntonioLunaAndrade","RafaelVegaHernández","ChristopherCristianCházaro","RubénMorenoArcher")))

# replace MORENA= Morena+MORENA, then drop Morena
df_collapsed <- df_collapsed %>%
  mutate(
    MORENA = if ("MORENA" %in% names(.)) {
      coalesce(MORENA,0) + coalesce(Morena,0)
    } else {
      coalesce(Morena,0)
    }
  ) %>%
  select(-any_of("Morena"))

# egen valid=rowtotal(PAN PRI PRD PVEM PT PANAL MC MORENA PES PRI_PVEM PAN_PRD CI_1 CI_2 CI_3)
df_collapsed <- df_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(any_of(c(
      "PAN","PRI","PRD","PVEM","PT","PANAL","MC","MORENA","PES","PRI_PVEM","PAN_PRD","CI_1","CI_2","CI_3"
    ))), na.rm=TRUE)
  ) %>%
  ungroup()

# gen total= valid + nulo + no_reg
df_collapsed <- df_collapsed %>%
  mutate(
    total = coalesce(valid,0) + coalesce(nulo,0) + coalesce(no_reg,0)
  ) %>%
  select(-nulo, -no_reg)

################################################################################
# 6) Merging with "uniqueids.xlsx" => "uniqueids16.dta", then "LN 2017", etc.
################################################################################

# We'll replicate with a simpler approach:
df_uniqueids <- read_excel("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/uniqueids.xlsx") %>%
  as.data.frame() %>% dplyr::select(-MUN)

df_merged <- df_collapsed %>%
  left_join(df_uniqueids, by="municipality")


# set year=2017, month="June", STATE="VERACRUZ"
df_merged <- df_merged %>%
  mutate(
    year  = 2017,
    month = "June",
    STATE = "VERACRUZ"
  )

################################################################################
# 7) Merging with LN2017
################################################################################

df_ln17 <- read_dta("../Listas Nominales/LN 2012-2019/2017/LN2017.dta") %>%
  # keep if entidad==30 & month==5, seccion!=0
  filter(entidad==30, month==5, seccion!=0) %>%
  mutate(
    uniqueid = (entidad*1000) + municipio
  ) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section=seccion)

# merge 1:1 section using LN17_VER.dta => we replicate a left_join 
df_final <- df_merged %>%
  left_join(df_ln17, by=c("uniqueid","section")) %>%
  filter(!is.na(lista))  # drop _merge==2

# rename lista -> listanominal
df_2017 <- df_final %>%
  rename(listanominal = lista) %>%
  mutate(
    turnout = total / listanominal
  )


################################################################################
# Part B: Reading "VERACRUZ_EXTRAORDINARIAMPAL_2018.xlsx", creating 2018 dataset
################################################################################

# 1) Read the Excel from cellrange(A7:AM138), firstrow, etc.
df_2018 <- read_excel(
  path = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016/VERACRUZ_EXTRAORDINARIAMPAL_2018.xlsx",
  sheet = "VERACRUZ_EXTRAORDINARIAMPAL_201",
  range = "A7:AM138",
  col_names = TRUE
) %>%
  as.data.frame()

# rename SECCION->section, MUNICIPIO->municipality, LISTA_NOMINAL->listanominal, TOTAL->total
df_2018 <- df_2018 %>%
  rename(
    section     = SECCION,
    municipality= MUNICIPIO,
    listanominal= LISTA_NOMINAL,
    total       = TOTAL
  )

# 2) Create new columns for coalitions
#    g PAN_PRD = PAN + PRD + PANPRD
#    g PRI_PVEM= PRI + PVEM + PRIPVEM
df_2018 <- df_2018 %>%
  mutate(
    PAN_PRD  = coalesce(PAN,0) + coalesce(PRD,0) + coalesce(PANPRD,0),
    PRI_PVEM = coalesce(PRI,0) + coalesce(PVEM,0) + coalesce(PRIPVEM,0)
  ) %>%
  # drop old columns
  select(-PAN, -PRD, -PANPRD, -PRI, -PVEM, -PRIPVEM)

# 3) If municipality=="EMILIANO ZAPATA", we define MORENA_PT_PES = ...
#    drop PT MORENA PTMORENAPES PTPES PTMORENA MORENAPES
#    replace PES=. if municipality=="EMILIANO ZAPATA"
df_2018 <- df_2018 %>%
  rowwise() %>%
  mutate(
    MORENA_PT_PES = if_else(
      municipality=="EMILIANO ZAPATA",
      coalesce(MORENA,0)+coalesce(PES,0)+coalesce(PT,0)+coalesce(PTMORENAPES,0)+coalesce(PTPES,0)+coalesce(PTMORENA,0)+coalesce(MORENAPES,0),
      NA_real_
    )
  ) %>%
  ungroup()

df_2018 <- df_2018 %>%
  mutate(
    PES = if_else(municipality=="EMILIANO ZAPATA", NA_real_, PES)
  ) %>%
  select(-PT, -MORENA, -PTMORENAPES, -PTPES, -PTMORENA, -MORENAPES)

# 4) collapse (sum) columns: PAN_PRD, PRI_PVEM, MORENA_PT_PES, PANAL, PES, listanominal, total by (municipality, section)
df_2018_collapsed <- df_2018 %>%
  group_by(municipality, section) %>%
  summarise(
    across(c(PAN_PRD, PRI_PVEM, MORENA_PT_PES, PANAL, PES, listanominal, total), sum, na.rm=TRUE),
    .groups="drop"
  )

# 5) g uniqueid=.
#    replace uniqueid=30007 if municipality=="CAMARON DE TEJEDA"
#    ...
df_2018_collapsed <- df_2018_collapsed %>%
  mutate(uniqueid = NA_real_) %>%
  mutate(
    uniqueid = case_when(
      municipality=="CAMARON DE TEJEDA" ~ 30007,
      municipality=="EMILIANO ZAPATA"   ~ 30065,
      municipality=="SAYULA DE ALEMAN"  ~ 30144,
      TRUE ~ uniqueid
    )
  )

# 6) replace municipality = municipality + " EXTRAORDINARIO"
df_2018_collapsed <- df_2018_collapsed %>%
  mutate(
    municipality = paste0(municipality, " EXTRAORDINARIO")
  )

# 7) gen turnout= total/listanominal
df_2018_collapsed <- df_2018_collapsed %>%
  mutate(turnout = total/listanominal)

# 8) egen valid=rowtotal(PAN_PRD PRI_PVEM MORENA_PT_PES PANAL PES)
#    gen year=2018, gen month="March"
df_2018_collapsed <- df_2018_collapsed %>%
  rowwise() %>%
  mutate(
    valid = sum(c_across(c(PAN_PRD, PRI_PVEM, MORENA_PT_PES, PANAL, PES)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year = 2018,
    month= "March"
  )

# Combine the dataframes, handling different columns by filling with NA
veracruz_all <- bind_rows(df_2004,
                          df_2010,
                          df_2013,
                          df_2017,
                          df_2018_collapsed
                          )

data.table::fwrite(veracruz_all,"../../../Processed Data/veracruz/Veracruz_process_raw_data.csv")


