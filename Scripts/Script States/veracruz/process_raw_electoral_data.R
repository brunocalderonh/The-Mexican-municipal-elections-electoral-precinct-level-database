################################################################################
## Process Raw Electoral Data - Veracruz
## Years: 2000, 2004, 2005_EXT, 2007, 2010, 2013, 2017, 2018_EXT
################################################################################

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, haven, readxl, tidyverse, data.table, stringr)

# Set working directory
<<<<<<< HEAD
setwd("D:/Dropbox/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Veracruz 2000, 2004, 2007, 2010, 2013")
=======
# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, ""))

################################################################################
## Helper Functions
################################################################################

remove_accents <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("Á|á", "A", x)
  x <- gsub("É|é", "E", x)
  x <- gsub("Í|í", "I", x)
  x <- gsub("Ó|ó", "O", x)
  x <- gsub("Ú|ú", "U", x)
  x <- gsub("Ñ|ñ", "N", x)
  x <- gsub("ü|Ü", "U", x)
  return(x)
}

# Comprehensive uniqueid assignment for Veracruz (212 municipalities)
assign_veracruz_uniqueid <- function(municipality) {
  municipality <- toupper(remove_accents(municipality))
  case_when(
    grepl("^ACAJETE$", municipality) ~ 30001,
    grepl("^ACATLAN$", municipality) ~ 30002,
    grepl("^ACAYUCAN$", municipality) ~ 30003,
    grepl("^ACTOPAN$", municipality) ~ 30004,
    grepl("^ACULA$", municipality) ~ 30005,
    grepl("^ACULTZINGO$", municipality) ~ 30006,
    grepl("CAMARON DE TEJEDA", municipality) ~ 30007,
    grepl("ALPATLAHUAC", municipality) ~ 30008,
    grepl("ALTO LUCERO", municipality) ~ 30009,
    grepl("^ALTOTONGA$", municipality) ~ 30010,
    grepl("^ALVARADO$", municipality) ~ 30011,
    grepl("^AMATITLAN$", municipality) ~ 30012,
    grepl("NARANJOS AMATLAN", municipality) ~ 30013,
    grepl("AMATLAN DE LOS REYES", municipality) ~ 30014,
    grepl("ANGEL R", municipality) ~ 30015,
    grepl("^LA ANTIGUA$", municipality) ~ 30016,
    grepl("^APAZAPAN$", municipality) ~ 30017,
    grepl("^AQUILA$", municipality) ~ 30018,
    grepl("^ASTACINGA$", municipality) ~ 30019,
    grepl("ATLAHUICO|ATLAHUILCO", municipality) ~ 30020,
    grepl("^ATOYAC$", municipality) ~ 30021,
    grepl("^ATZACAN$", municipality) ~ 30022,
    grepl("^ATZALAN$", municipality) ~ 30023,
    grepl("TLALTETELA", municipality) ~ 30024,
    grepl("AYAHUALULCO", municipality) ~ 30025,
    grepl("^BANDERILLA$", municipality) ~ 30026,
    grepl("^BENITO JUAREZ$", municipality) ~ 30027,
    grepl("BOCA DEL RIO", municipality) ~ 30028,
    grepl("CALCAHUALCO", municipality) ~ 30029,
    grepl("CAMERINO Z", municipality) ~ 30030,
    grepl("CARRILLO PUERTO", municipality) ~ 30031,
    grepl("^CATEMACO$", municipality) ~ 30032,
    grepl("CAZONES", municipality) ~ 30033,
    grepl("CERRO AZUL", municipality) ~ 30034,
    grepl("CITLALT", municipality) ~ 30035,
    grepl("COACOATZINTLA", municipality) ~ 30036,
    grepl("COAHUITLAN", municipality) ~ 30037,
    grepl("^COATEPEC$", municipality) ~ 30038,
    grepl("COATZACOALCOS", municipality) ~ 30039,
    grepl("COATZINTLA", municipality) ~ 30040,
    grepl("COETZALA", municipality) ~ 30041,
    grepl("^COLIPA$", municipality) ~ 30042,
    grepl("^COMAPA$", municipality) ~ 30043,
    grepl("CORDOBA", municipality) ~ 30044,
    grepl("COSAMALOAPAN", municipality) ~ 30045,
    grepl("COSAUTLAN", municipality) ~ 30046,
    grepl("COSCOMATEPEC", municipality) ~ 30047,
    grepl("COSOLEACAQUE", municipality) ~ 30048,
    grepl("COTAXTLA", municipality) ~ 30049,
    grepl("COXQUIHUI", municipality) ~ 30050,
    grepl("COYUTLA", municipality) ~ 30051,
    grepl("^CUICHAPA$", municipality) ~ 30052,
    grepl("CUITLAHUAC", municipality) ~ 30053,
    grepl("CHACALTIANGUIS", municipality) ~ 30054,
    grepl("^CHALMA$", municipality) ~ 30055,
    grepl("CHICONAMEL", municipality) ~ 30056,
    grepl("CHICONQUIACO", municipality) ~ 30057,
    grepl("CHICONTEPEC", municipality) ~ 30058,
    grepl("^CHINAMECA$", municipality) ~ 30059,
    grepl("CHINAMPA", municipality) ~ 30060,
    grepl("LAS CHOAPAS", municipality) ~ 30061,
    grepl("CHOCAMAN", municipality) ~ 30062,
    grepl("^CHONTLA$", municipality) ~ 30063,
    grepl("CHUMATLAN", municipality) ~ 30064,
    grepl("^EMILIANO ZAPATA", municipality) ~ 30065,
    grepl("^ESPINAL$", municipality) ~ 30066,
    grepl("FILOMENO MATA", municipality) ~ 30067,
    grepl("^FORTIN$", municipality) ~ 30068,
    grepl("GUTIERREZ ZAMORA", municipality) ~ 30069,
    grepl("HIDALGOTITLAN", municipality) ~ 30070,
    grepl("^HUATUSCO$", municipality) ~ 30071,
    grepl("HUAYACOCOTLA", municipality) ~ 30072,
    grepl("HUEYAPAN", municipality) ~ 30073,
    grepl("HUILOAPAN", municipality) ~ 30074,
    grepl("IGNACIO DE LA LLAVE", municipality) ~ 30075,
    grepl("ILAMATLAN", municipality) ~ 30076,
    grepl("^ISLA$", municipality) ~ 30077,
    grepl("IXCATEPEC", municipality) ~ 30078,
    grepl("IXHUACAN DE LOS REYES|DE LOS REYES", municipality) & grepl("IXHUA", municipality) ~ 30079,
    grepl("IXHUATLAN DEL CAF", municipality) ~ 30080,
    grepl("IXHUATLANCILLO", municipality) ~ 30081,
    grepl("IXHUATLAN DEL SURESTE", municipality) ~ 30082,
    grepl("IXHUATLAN DE MADERO", municipality) ~ 30083,
    grepl("IXMATLAHUACAN", municipality) ~ 30084,
    grepl("IXTACZOQUITLAN", municipality) ~ 30085,
    grepl("^JALACINGO$", municipality) ~ 30086,
    grepl("^XALAPA$", municipality) ~ 30087,
    grepl("JALCOMULCO", municipality) ~ 30088,
    grepl("^JALTIPAN$", municipality) ~ 30089,
    grepl("^JAMAPA$", municipality) ~ 30090,
    grepl("JESUS CARRANZA", municipality) ~ 30091,
    grepl("^XICO$", municipality) ~ 30092,
    grepl("^JILOTEPEC$", municipality) ~ 30093,
    grepl("JUAN RODRIGUEZ CLARA", municipality) ~ 30094,
    grepl("JUCHIQUE DE FERRER", municipality) ~ 30095,
    grepl("LANDERO Y COSS", municipality) ~ 30096,
    grepl("LERDO DE TEJADA", municipality) ~ 30097,
    grepl("^MAGDALENA$", municipality) ~ 30098,
    grepl("^MALTRATA$", municipality) ~ 30099,
    grepl("MANLIO FABIO", municipality) ~ 30100,
    grepl("MARIANO ESCOBEDO", municipality) ~ 30101,
    grepl("MARTINEZ DE LA TORRE", municipality) ~ 30102,
    grepl("MECATLAN", municipality) ~ 30103,
    grepl("MECAYAPAN", municipality) ~ 30104,
    grepl("^MEDELLIN$", municipality) ~ 30105,
    grepl("MIAHUATLAN", municipality) ~ 30106,
    grepl("LAS MINAS", municipality) ~ 30107,
    grepl("MINATITLAN", municipality) ~ 30108,
    grepl("^MISANTLA$", municipality) ~ 30109,
    grepl("MIXTLA DE ALTAMIRANO", municipality) ~ 30110,
    grepl("MOLOACAN", municipality) ~ 30111,
    grepl("^NAOLINCO$", municipality) ~ 30112,
    grepl("^NARANJAL$", municipality) ~ 30113,
    grepl("^NAUTLA$", municipality) ~ 30114,
    grepl("^NOGALES$", municipality) ~ 30115,
    grepl("^OLUTA$", municipality) ~ 30116,
    grepl("^OMEALCA$", municipality) ~ 30117,
    grepl("^ORIZABA$", municipality) ~ 30118,
    grepl("OTATITLAN", municipality) ~ 30119,
    grepl("^OTEAPAN$", municipality) ~ 30120,
    grepl("OZULUAMA", municipality) ~ 30121,
    grepl("^PAJAPAN$", municipality) ~ 30122,
    grepl("^PANUCO$", municipality) ~ 30123,
    grepl("^PAPANTLA$", municipality) ~ 30124,
    grepl("PASO DEL MACHO", municipality) ~ 30125,
    grepl("PASO DE OVEJAS", municipality) ~ 30126,
    grepl("LA PERLA", municipality) ~ 30127,
    grepl("^PEROTE$", municipality) ~ 30128,
    grepl("PLATON SANCHEZ|PLAT.N S.NCHEZ", municipality) ~ 30129,
    grepl("PLAYA VICENTE", municipality) ~ 30130,
    grepl("POZA RICA|POZARICA", municipality) ~ 30131,
    grepl("LAS VIGAS", municipality) ~ 30132,
    grepl("PUEBLO VIEJO", municipality) ~ 30133,
    grepl("PUENTE NACIONAL", municipality) ~ 30134,
    grepl("RAFAEL DELGADO", municipality) ~ 30135,
    grepl("RAFAEL LUCIO", municipality) ~ 30136,
    grepl("LOS REYES", municipality) & !grepl("IXHUA", municipality) ~ 30137,
    grepl("RIO BLANCO", municipality) ~ 30138,
    grepl("SALTABARRANCA", municipality) ~ 30139,
    grepl("SAN ANDRES TENEJAPAN", municipality) ~ 30140,
    grepl("SAN ANDRES TUXTLA", municipality) ~ 30141,
    grepl("SAN JUAN EVANGELISTA", municipality) ~ 30142,
    grepl("SANTIAGO TUXTLA", municipality) ~ 30143,
    grepl("SAYULA DE ALEMAN", municipality) ~ 30144,
    grepl("^SOCONUSCO$", municipality) ~ 30145,
    grepl("^SOCHIAPA$", municipality) ~ 30146,
    grepl("SOLEDAD ATZOMPA", municipality) ~ 30147,
    grepl("SOLEDAD DE DOBLADO", municipality) ~ 30148,
    grepl("SOTEAPAN", municipality) ~ 30149,
    grepl("^TAMALIN$", municipality) ~ 30150,
    grepl("^TAMIAHUA$", municipality) ~ 30151,
    grepl("TAMPICO ALTO", municipality) ~ 30152,
    grepl("^TANCOCO$", municipality) ~ 30153,
    grepl("^TANTIMA$", municipality) ~ 30154,
    grepl("^TANTOYUCA$", municipality) ~ 30155,
    grepl("^TATATILA$", municipality) ~ 30156,
    grepl("CASTILLO DE TEAYO", municipality) ~ 30157,
    grepl("^TECOLUTLA$", municipality) ~ 30158,
    grepl("TEHUIPANGO", municipality) ~ 30159,
    grepl("TEMAPACHE|^ALAMO$", municipality) ~ 30160,
    grepl("^TEMPOAL$", municipality) ~ 30161,
    grepl("^TENAMPA$", municipality) ~ 30162,
    grepl("TENOCHTITLAN", municipality) ~ 30163,
    grepl("^TEOCELO$", municipality) ~ 30164,
    grepl("TEPATLAXCO", municipality) ~ 30165,
    grepl("^TEPETLAN$", municipality) ~ 30166,
    grepl("TEPETZINTLA", municipality) ~ 30167,
    grepl("^TEQUILA$", municipality) ~ 30168,
    grepl("JOSE AZUETA", municipality) ~ 30169,
    grepl("TEXCATEPEC", municipality) ~ 30170,
    grepl("TEXHUACAN", municipality) ~ 30171,
    grepl("TEXISTEPEC", municipality) ~ 30172,
    grepl("TEZONAPA", municipality) ~ 30173,
    grepl("TIERRA BLANCA", municipality) ~ 30174,
    grepl("TIHUATLAN", municipality) ~ 30175,
    grepl("TLACOJALPAN", municipality) ~ 30176,
    grepl("TLACOLULAN", municipality) ~ 30177,
    grepl("TLACOTALPAN", municipality) ~ 30178,
    grepl("TLACOTEPEC DE MEJIA", municipality) ~ 30179,
    grepl("TLACHICHILCO", municipality) ~ 30180,
    grepl("TLALIXCOYAN", municipality) ~ 30181,
    grepl("TLALNELHUAYOCAN", municipality) ~ 30182,
    grepl("TLAPACOYAN", municipality) ~ 30183,
    grepl("TLAQUILPA", municipality) ~ 30184,
    grepl("TLILAPAN", municipality) ~ 30185,
    grepl("^TOMATLAN$", municipality) ~ 30186,
    grepl("^TONAYAN$", municipality) ~ 30187,
    grepl("^TOTUTLA$", municipality) ~ 30188,
    grepl("^TUXPAN$", municipality) ~ 30189,
    grepl("^TUXTILLA$", municipality) ~ 30190,
    grepl("URSULO GALVAN", municipality) ~ 30191,
    grepl("VEGA DE ALATORRE", municipality) ~ 30192,
    grepl("^VERACRUZ$", municipality) ~ 30193,
    grepl("VILLA ALDAMA", municipality) ~ 30194,
    grepl("XOXOCOTLA", municipality) ~ 30195,
    grepl("^YANGA$", municipality) ~ 30196,
    grepl("YECUATLA", municipality) ~ 30197,
    grepl("ZACUALPAN", municipality) ~ 30198,
    grepl("^ZARAGOZA$", municipality) ~ 30199,
    grepl("^ZENTLA$", municipality) ~ 30200,
    grepl("^ZONGOLICA$", municipality) ~ 30201,
    grepl("ZONTECOMATLAN", municipality) ~ 30202,
    grepl("ZOZOCOLCO", municipality) ~ 30203,
    grepl("AGUA DULCE", municipality) ~ 30204,
    grepl("EL HIGO", municipality) ~ 30205,
    grepl("NANCHITAL", municipality) ~ 30206,
    grepl("TRES VALLES", municipality) ~ 30207,
    grepl("CARLOS A. CARRILLO", municipality) ~ 30208,
    grepl("TATAHUICAPAN", municipality) ~ 30209,
    grepl("UXPANAPA", municipality) ~ 30210,
    grepl("SAN RAFAEL", municipality) ~ 30211,
    grepl("SANTIAGO SOCHIAPAN", municipality) ~ 30212,
    TRUE ~ NA_real_
  )
}

>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

################################################################################
## Helper Functions
################################################################################

remove_accents <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("Á|á", "A", x)
  x <- gsub("É|é", "E", x)
  x <- gsub("Í|í", "I", x)
  x <- gsub("Ó|ó", "O", x)
  x <- gsub("Ú|ú", "U", x)
  x <- gsub("Ñ|ñ", "N", x)
  x <- gsub("ü|Ü", "U", x)
  return(x)
}

<<<<<<< HEAD
# Comprehensive uniqueid assignment for Veracruz (212 municipalities)
assign_veracruz_uniqueid <- function(municipality) {
  municipality <- toupper(remove_accents(municipality))
  case_when(
    grepl("^ACAJETE$", municipality) ~ 30001,
    grepl("^ACATLAN$", municipality) ~ 30002,
    grepl("^ACAYUCAN$", municipality) ~ 30003,
    grepl("^ACTOPAN$", municipality) ~ 30004,
    grepl("^ACULA$", municipality) ~ 30005,
    grepl("^ACULTZINGO$", municipality) ~ 30006,
    grepl("CAMARON DE TEJEDA", municipality) ~ 30007,
    grepl("ALPATLAHUAC", municipality) ~ 30008,
    grepl("ALTO LUCERO", municipality) ~ 30009,
    grepl("^ALTOTONGA$", municipality) ~ 30010,
    grepl("^ALVARADO$", municipality) ~ 30011,
    grepl("^AMATITLAN$", municipality) ~ 30012,
    grepl("NARANJOS AMATLAN", municipality) ~ 30013,
    grepl("AMATLAN DE LOS REYES", municipality) ~ 30014,
    grepl("ANGEL R", municipality) ~ 30015,
    grepl("^LA ANTIGUA$", municipality) ~ 30016,
    grepl("^APAZAPAN$", municipality) ~ 30017,
    grepl("^AQUILA$", municipality) ~ 30018,
    grepl("^ASTACINGA$", municipality) ~ 30019,
    grepl("ATLAHUICO|ATLAHUILCO", municipality) ~ 30020,
    grepl("^ATOYAC$", municipality) ~ 30021,
    grepl("^ATZACAN$", municipality) ~ 30022,
    grepl("^ATZALAN$", municipality) ~ 30023,
    grepl("TLALTETELA", municipality) ~ 30024,
    grepl("AYAHUALULCO", municipality) ~ 30025,
    grepl("^BANDERILLA$", municipality) ~ 30026,
    grepl("^BENITO JUAREZ$", municipality) ~ 30027,
    grepl("BOCA DEL RIO", municipality) ~ 30028,
    grepl("CALCAHUALCO", municipality) ~ 30029,
    grepl("CAMERINO Z", municipality) ~ 30030,
    grepl("CARRILLO PUERTO", municipality) ~ 30031,
    grepl("^CATEMACO$", municipality) ~ 30032,
    grepl("CAZONES", municipality) ~ 30033,
    grepl("CERRO AZUL", municipality) ~ 30034,
    grepl("CITLALT", municipality) ~ 30035,
    grepl("COACOATZINTLA", municipality) ~ 30036,
    grepl("COAHUITLAN", municipality) ~ 30037,
    grepl("^COATEPEC$", municipality) ~ 30038,
    grepl("COATZACOALCOS", municipality) ~ 30039,
    grepl("COATZINTLA", municipality) ~ 30040,
    grepl("COETZALA", municipality) ~ 30041,
    grepl("^COLIPA$", municipality) ~ 30042,
    grepl("^COMAPA$", municipality) ~ 30043,
    grepl("CORDOBA", municipality) ~ 30044,
    grepl("COSAMALOAPAN", municipality) ~ 30045,
    grepl("COSAUTLAN", municipality) ~ 30046,
    grepl("COSCOMATEPEC", municipality) ~ 30047,
    grepl("COSOLEACAQUE", municipality) ~ 30048,
    grepl("COTAXTLA", municipality) ~ 30049,
    grepl("COXQUIHUI", municipality) ~ 30050,
    grepl("COYUTLA", municipality) ~ 30051,
    grepl("^CUICHAPA$", municipality) ~ 30052,
    grepl("CUITLAHUAC", municipality) ~ 30053,
    grepl("CHACALTIANGUIS", municipality) ~ 30054,
    grepl("^CHALMA$", municipality) ~ 30055,
    grepl("CHICONAMEL", municipality) ~ 30056,
    grepl("CHICONQUIACO", municipality) ~ 30057,
    grepl("CHICONTEPEC", municipality) ~ 30058,
    grepl("^CHINAMECA$", municipality) ~ 30059,
    grepl("CHINAMPA", municipality) ~ 30060,
    grepl("LAS CHOAPAS", municipality) ~ 30061,
    grepl("CHOCAMAN", municipality) ~ 30062,
    grepl("^CHONTLA$", municipality) ~ 30063,
    grepl("CHUMATLAN", municipality) ~ 30064,
    grepl("^EMILIANO ZAPATA", municipality) ~ 30065,
    grepl("^ESPINAL$", municipality) ~ 30066,
    grepl("FILOMENO MATA", municipality) ~ 30067,
    grepl("^FORTIN$", municipality) ~ 30068,
    grepl("GUTIERREZ ZAMORA", municipality) ~ 30069,
    grepl("HIDALGOTITLAN", municipality) ~ 30070,
    grepl("^HUATUSCO$", municipality) ~ 30071,
    grepl("HUAYACOCOTLA", municipality) ~ 30072,
    grepl("HUEYAPAN", municipality) ~ 30073,
    grepl("HUILOAPAN", municipality) ~ 30074,
    grepl("IGNACIO DE LA LLAVE", municipality) ~ 30075,
    grepl("ILAMATLAN", municipality) ~ 30076,
    grepl("^ISLA$", municipality) ~ 30077,
    grepl("IXCATEPEC", municipality) ~ 30078,
    grepl("IXHUACAN DE LOS REYES|DE LOS REYES", municipality) & grepl("IXHUA", municipality) ~ 30079,
    grepl("IXHUATLAN DEL CAF", municipality) ~ 30080,
    grepl("IXHUATLANCILLO", municipality) ~ 30081,
    grepl("IXHUATLAN DEL SURESTE", municipality) ~ 30082,
    grepl("IXHUATLAN DE MADERO", municipality) ~ 30083,
    grepl("IXMATLAHUACAN", municipality) ~ 30084,
    grepl("IXTACZOQUITLAN", municipality) ~ 30085,
    grepl("^JALACINGO$", municipality) ~ 30086,
    grepl("^XALAPA$", municipality) ~ 30087,
    grepl("JALCOMULCO", municipality) ~ 30088,
    grepl("^JALTIPAN$", municipality) ~ 30089,
    grepl("^JAMAPA$", municipality) ~ 30090,
    grepl("JESUS CARRANZA", municipality) ~ 30091,
    grepl("^XICO$", municipality) ~ 30092,
    grepl("^JILOTEPEC$", municipality) ~ 30093,
    grepl("JUAN RODRIGUEZ CLARA", municipality) ~ 30094,
    grepl("JUCHIQUE DE FERRER", municipality) ~ 30095,
    grepl("LANDERO Y COSS", municipality) ~ 30096,
    grepl("LERDO DE TEJADA", municipality) ~ 30097,
    grepl("^MAGDALENA$", municipality) ~ 30098,
    grepl("^MALTRATA$", municipality) ~ 30099,
    grepl("MANLIO FABIO", municipality) ~ 30100,
    grepl("MARIANO ESCOBEDO", municipality) ~ 30101,
    grepl("MARTINEZ DE LA TORRE", municipality) ~ 30102,
    grepl("MECATLAN", municipality) ~ 30103,
    grepl("MECAYAPAN", municipality) ~ 30104,
    grepl("^MEDELLIN$", municipality) ~ 30105,
    grepl("MIAHUATLAN", municipality) ~ 30106,
    grepl("LAS MINAS", municipality) ~ 30107,
    grepl("MINATITLAN", municipality) ~ 30108,
    grepl("^MISANTLA$", municipality) ~ 30109,
    grepl("MIXTLA DE ALTAMIRANO", municipality) ~ 30110,
    grepl("MOLOACAN", municipality) ~ 30111,
    grepl("^NAOLINCO$", municipality) ~ 30112,
    grepl("^NARANJAL$", municipality) ~ 30113,
    grepl("^NAUTLA$", municipality) ~ 30114,
    grepl("^NOGALES$", municipality) ~ 30115,
    grepl("^OLUTA$", municipality) ~ 30116,
    grepl("^OMEALCA$", municipality) ~ 30117,
    grepl("^ORIZABA$", municipality) ~ 30118,
    grepl("OTATITLAN", municipality) ~ 30119,
    grepl("^OTEAPAN$", municipality) ~ 30120,
    grepl("OZULUAMA", municipality) ~ 30121,
    grepl("^PAJAPAN$", municipality) ~ 30122,
    grepl("^PANUCO$", municipality) ~ 30123,
    grepl("^PAPANTLA$", municipality) ~ 30124,
    grepl("PASO DEL MACHO", municipality) ~ 30125,
    grepl("PASO DE OVEJAS", municipality) ~ 30126,
    grepl("LA PERLA", municipality) ~ 30127,
    grepl("^PEROTE$", municipality) ~ 30128,
    grepl("PLATON SANCHEZ|PLAT.N S.NCHEZ", municipality) ~ 30129,
    grepl("PLAYA VICENTE", municipality) ~ 30130,
    grepl("POZA RICA|POZARICA", municipality) ~ 30131,
    grepl("LAS VIGAS", municipality) ~ 30132,
    grepl("PUEBLO VIEJO", municipality) ~ 30133,
    grepl("PUENTE NACIONAL", municipality) ~ 30134,
    grepl("RAFAEL DELGADO", municipality) ~ 30135,
    grepl("RAFAEL LUCIO", municipality) ~ 30136,
    grepl("LOS REYES", municipality) & !grepl("IXHUA", municipality) ~ 30137,
    grepl("RIO BLANCO", municipality) ~ 30138,
    grepl("SALTABARRANCA", municipality) ~ 30139,
    grepl("SAN ANDRES TENEJAPAN", municipality) ~ 30140,
    grepl("SAN ANDRES TUXTLA", municipality) ~ 30141,
    grepl("SAN JUAN EVANGELISTA", municipality) ~ 30142,
    grepl("SANTIAGO TUXTLA", municipality) ~ 30143,
    grepl("SAYULA DE ALEMAN", municipality) ~ 30144,
    grepl("^SOCONUSCO$", municipality) ~ 30145,
    grepl("^SOCHIAPA$", municipality) ~ 30146,
    grepl("SOLEDAD ATZOMPA", municipality) ~ 30147,
    grepl("SOLEDAD DE DOBLADO", municipality) ~ 30148,
    grepl("SOTEAPAN", municipality) ~ 30149,
    grepl("^TAMALIN$", municipality) ~ 30150,
    grepl("^TAMIAHUA$", municipality) ~ 30151,
    grepl("TAMPICO ALTO", municipality) ~ 30152,
    grepl("^TANCOCO$", municipality) ~ 30153,
    grepl("^TANTIMA$", municipality) ~ 30154,
    grepl("^TANTOYUCA$", municipality) ~ 30155,
    grepl("^TATATILA$", municipality) ~ 30156,
    grepl("CASTILLO DE TEAYO", municipality) ~ 30157,
    grepl("^TECOLUTLA$", municipality) ~ 30158,
    grepl("TEHUIPANGO", municipality) ~ 30159,
    grepl("TEMAPACHE|^ALAMO$", municipality) ~ 30160,
    grepl("^TEMPOAL$", municipality) ~ 30161,
    grepl("^TENAMPA$", municipality) ~ 30162,
    grepl("TENOCHTITLAN", municipality) ~ 30163,
    grepl("^TEOCELO$", municipality) ~ 30164,
    grepl("TEPATLAXCO", municipality) ~ 30165,
    grepl("^TEPETLAN$", municipality) ~ 30166,
    grepl("TEPETZINTLA", municipality) ~ 30167,
    grepl("^TEQUILA$", municipality) ~ 30168,
    grepl("JOSE AZUETA", municipality) ~ 30169,
    grepl("TEXCATEPEC", municipality) ~ 30170,
    grepl("TEXHUACAN", municipality) ~ 30171,
    grepl("TEXISTEPEC", municipality) ~ 30172,
    grepl("TEZONAPA", municipality) ~ 30173,
    grepl("TIERRA BLANCA", municipality) ~ 30174,
    grepl("TIHUATLAN", municipality) ~ 30175,
    grepl("TLACOJALPAN", municipality) ~ 30176,
    grepl("TLACOLULAN", municipality) ~ 30177,
    grepl("TLACOTALPAN", municipality) ~ 30178,
    grepl("TLACOTEPEC DE MEJIA", municipality) ~ 30179,
    grepl("TLACHICHILCO", municipality) ~ 30180,
    grepl("TLALIXCOYAN", municipality) ~ 30181,
    grepl("TLALNELHUAYOCAN", municipality) ~ 30182,
    grepl("TLAPACOYAN", municipality) ~ 30183,
    grepl("TLAQUILPA", municipality) ~ 30184,
    grepl("TLILAPAN", municipality) ~ 30185,
    grepl("^TOMATLAN$", municipality) ~ 30186,
    grepl("^TONAYAN$", municipality) ~ 30187,
    grepl("^TOTUTLA$", municipality) ~ 30188,
    grepl("^TUXPAN$", municipality) ~ 30189,
    grepl("^TUXTILLA$", municipality) ~ 30190,
    grepl("URSULO GALVAN", municipality) ~ 30191,
    grepl("VEGA DE ALATORRE", municipality) ~ 30192,
    grepl("^VERACRUZ$", municipality) ~ 30193,
    grepl("VILLA ALDAMA", municipality) ~ 30194,
    grepl("XOXOCOTLA", municipality) ~ 30195,
    grepl("^YANGA$", municipality) ~ 30196,
    grepl("YECUATLA", municipality) ~ 30197,
    grepl("ZACUALPAN", municipality) ~ 30198,
    grepl("^ZARAGOZA$", municipality) ~ 30199,
    grepl("^ZENTLA$", municipality) ~ 30200,
    grepl("^ZONGOLICA$", municipality) ~ 30201,
    grepl("ZONTECOMATLAN", municipality) ~ 30202,
    grepl("ZOZOCOLCO", municipality) ~ 30203,
    grepl("AGUA DULCE", municipality) ~ 30204,
    grepl("EL HIGO", municipality) ~ 30205,
    grepl("NANCHITAL", municipality) ~ 30206,
    grepl("TRES VALLES", municipality) ~ 30207,
    grepl("CARLOS A. CARRILLO", municipality) ~ 30208,
    grepl("TATAHUICAPAN", municipality) ~ 30209,
    grepl("UXPANAPA", municipality) ~ 30210,
    grepl("SAN RAFAEL", municipality) ~ 30211,
    grepl("SANTIAGO SOCHIAPAN", municipality) ~ 30212,
    TRUE ~ NA_real_
  )
}
=======
df_listanom <- read_excel(
  path = "../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Lisado Nominal 2004.xlsx",
  sheet = "Sheet1",
  col_names = TRUE)

# keep only (section, listanominal)
df_listanom <- df_listanom %>%
  select(section, listanominal)

# drop if section==. & listanominal==.
df_listanom <- df_listanom %>%
  filter(!(is.na(section) & is.na(listanominal)))

# sort section => in R we do:
df_listanom <- df_listanom %>%
  arrange(section)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

################################################################################
## 2000 - Loaded from external script (DoFile_Section_Veracruz2000.do)
## If this file exists, it would be processed separately
################################################################################

# Placeholder for 2000 - typically would source external do-file
# df_2000 <- ... (process from DoFile_Section_Veracruz2000.do)
cat("Note: 2000 data should be processed from DoFile_Section_Veracruz2000.do\n")

################################################################################
## 2004
################################################################################

# Read lista nominal
ln_2004 <- read_excel("Lisado Nominal 2004.xlsx", sheet = "Sheet1") %>%
  select(section, listanominal) %>%
  filter(!is.na(section) & !is.na(listanominal))

# Read main data
df_2004 <- read.csv("Ayu_Seccion_2004_No_LN.csv", stringsAsFactors = FALSE)
names(df_2004) <- tolower(names(df_2004))

df_2004 <- df_2004 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Collapse
df_2004 <- df_2004 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Calculate total
df_2004 <- df_2004 %>%
  mutate(total = rowSums(select(., pan, fide, prd, prv, no_reg, nulos), na.rm = TRUE)) %>%
  filter(!is.na(total) & total > 0)

# Rename parties
df_2004 <- df_2004 %>%
  rename(PAN = pan,
         PRI_PVEM = fide,
         PRD_PT_PC = prd,
         PRV = prv) %>%
  select(-nulos, -no_reg)

# Merge lista nominal
df_2004 <- df_2004 %>%
  left_join(ln_2004, by = "section")

df_2004 <- df_2004 %>%
  mutate(turnout = total / listanominal)

# Assign uniqueid
df_2004 <- df_2004 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
df_2004 <- df_2004 %>%
  mutate(valid = rowSums(select(., PAN, PRI_PVEM, PRD_PT_PC, PRV), na.rm = TRUE))

# Municipal aggregates
df_2004 <- df_2004 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2004 <- df_2004 %>%
  mutate(year = 2004,
         month = "September")

cat("2004 processed:", nrow(df_2004), "rows\n")

################################################################################
## 2005 Extraordinary - LANDERO Y COSS (uniqueid 30096)
################################################################################

df_2005 <- read_excel("Extraordinario 2005.xlsx", sheet = "Sheet1")

df_2005 <- df_2005 %>%
  mutate(section = as.numeric(CASILLA),
         section = ifelse(row_number() >= 2, 2220, section)) %>%
  mutate(total = rowSums(select(., PAN, PRI, PT, NR, NULO), na.rm = TRUE))

df_2005 <- df_2005 %>%
  group_by(municipality, section) %>%
  summarise(PAN = sum(PAN, na.rm = TRUE),
            PRI = sum(PRI, na.rm = TRUE),
            PT = sum(PT, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(uniqueid = 30096)

# Merge lista nominal
all_months_2005 <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 30, month == 6, year == 2005) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2005 <- df_2005 %>%
  left_join(all_months_2005, by = "section")

df_2005 <- df_2005 %>%
  mutate(turnout = total / listanominal,
         valid = rowSums(select(., PAN, PRI, PT), na.rm = TRUE))

df_2005 <- df_2005 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2005 <- df_2005 %>%
  mutate(year = 2005,
         month = "June")

cat("2005 Extraordinary processed:", nrow(df_2005), "rows\n")

################################################################################
## 2007
################################################################################

# Read lista nominal
ln_2007 <- read_excel("Lisado Nominal 2007.xlsx", sheet = "Sheet1") %>%
  rename_with(~"section", matches("SECC|SECCIÓN")) %>%
  rename(listanominal = LISTA) %>%
  filter(!is.na(section) & section != "total Distrital" & !is.na(listanominal)) %>%
  mutate(section = as.numeric(section))

# Read main data
df_2007 <- read.csv("Ayu_Seccion_2007_No_LN.csv", stringsAsFactors = FALSE)
names(df_2007) <- tolower(names(df_2007))

df_2007 <- df_2007 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Collapse
df_2007 <- df_2007 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  filter(total > 0)

# Rename parties
df_2007 <- df_2007 %>%
  rename(PAN = pan,
         PRI_PVEM = pripvem,
         PRI_PVEM_PANAL = pripvempanal,
         PRD = prd,
         PRD_PT_PC = prdptpc,
         PT_PC = ptpc,
         PT = pt,
         PC = pc,
         PRV = prv,
         PAS = pas,
         PANAL = panal,
         MC = mc)

# Special correction for HUEYAPAN DE OCAMPO
df_2007 <- df_2007 %>%
  mutate(PRI_PVEM_PANAL = ifelse(municipality == "HUEYAPAN DE OCAMPO", 
                                  PRI_PVEM_PANAL + PANAL, PRI_PVEM_PANAL),
         PANAL = ifelse(municipality == "HUEYAPAN DE OCAMPO", 0, PANAL))

# Merge lista nominal
df_2007 <- df_2007 %>%
  left_join(ln_2007, by = "section")

df_2007 <- df_2007 %>%
  mutate(turnout = total / listanominal)

# Drop nulos and noregistrados
df_2007 <- df_2007 %>%
  select(-matches("^nulos$|^noregistrados$", ignore.case = TRUE))

# Assign uniqueid
df_2007 <- df_2007 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2007 <- c("PAN", "PRI_PVEM_PANAL", "PRI_PVEM", "PRD", "PRD_PT_PC", 
                     "PT_PC", "PT", "PC", "PRV", "PAS", "PANAL", "MC")
party_cols_2007 <- party_cols_2007[party_cols_2007 %in% names(df_2007)]

df_2007 <- df_2007 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2007)), na.rm = TRUE))

# Municipal aggregates
df_2007 <- df_2007 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2007 <- df_2007 %>%
  mutate(year = 2007,
         month = "September")

cat("2007 processed:", nrow(df_2007), "rows\n")

################################################################################
## 2010
################################################################################

# Read lista nominal
ln_2010 <- read_excel("Lisado Nominal 2010.xlsx", sheet = "Sheet1") %>%
  rename(section = SECCION,
         listanominal = LISTA) %>%
  filter(!is.na(section) & !is.na(listanominal)) %>%
  select(section, listanominal)

# Read main data
df_2010 <- read.csv("Ayu_Seccion_2010_No_LN.csv", stringsAsFactors = FALSE)
names(df_2010) <- tolower(names(df_2010))

df_2010 <- df_2010 %>%
  rename(municipality = municipio,
         section = secc) %>%
  filter(!(is.na(municipality) | municipality == "") | !is.na(section)) %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.)))) %>%
  filter(!is.na(total) & total > 0)

# Collapse
df_2010 <- df_2010 %>%
  group_by(municipality, section) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)), .groups = "drop")

# Create section-level dummies for coalition presence
df_2010 <- df_2010 %>%
  mutate(sec_dummy_pan_panal = ifelse(!is.na(panpanal) & panpanal > 0, 1, 0),
         sec_dummy_pri_pvem_prv = ifelse(!is.na(pripvemprv) & pripvemprv > 0, 1, 0),
         sec_dummy_prd_pt_pc = ifelse(!is.na(prdptpc) & prdptpc > 0, 1, 0))

# Calculate municipality-level dummies
df_2010 <- df_2010 %>%
  group_by(municipality) %>%
<<<<<<< HEAD
  mutate(dummy_pan_panal = max(sec_dummy_pan_panal, na.rm = TRUE),
         dummy_pri_pvem_prv = max(sec_dummy_pri_pvem_prv, na.rm = TRUE),
         dummy_prd_pt_pc = max(sec_dummy_prd_pt_pc, na.rm = TRUE)) %>%
  ungroup()

# Special correction for JALCOMULCO
df_2010 <- df_2010 %>%
  mutate(dummy_pri_pvem_prv = ifelse(municipality == "JALCOMULCO", 1, dummy_pri_pvem_prv))

# Create coalition columns
df_2010 <- df_2010 %>%
  mutate(
    PAN_PANAL = ifelse(dummy_pan_panal == 1, pan + panal + panpanal, NA),
    pan = ifelse(dummy_pan_panal == 1, NA, pan),
    panal = ifelse(dummy_pan_panal == 1, NA, panal),
    
    PRI_PVEM_PRV = ifelse(dummy_pri_pvem_prv == 1, pripvemprv + pri + pvem + prv, NA),
    pri = ifelse(dummy_pri_pvem_prv == 1, NA, pri),
    pvem = ifelse(dummy_pri_pvem_prv == 1, NA, pvem),
    prv = ifelse(dummy_pri_pvem_prv == 1, NA, prv),
    
    PRD_PT_PC = ifelse(dummy_prd_pt_pc == 1, prdptpc + prd + pt + pc, NA),
    prd = ifelse(dummy_prd_pt_pc == 1, NA, prd),
    pt = ifelse(dummy_prd_pt_pc == 1, NA, pt),
    pc = ifelse(dummy_prd_pt_pc == 1, NA, pc)
  )

# Rename remaining columns
df_2010 <- df_2010 %>%
  rename(PAN = pan,
         PRD = prd,
         PT = pt,
         PC = pc,
         PANAL = panal)

# Merge lista nominal
df_2010 <- df_2010 %>%
  left_join(ln_2010, by = "section")

df_2010 <- df_2010 %>%
  mutate(turnout = total / listanominal)

# Drop unnecessary columns
df_2010 <- df_2010 %>%
  select(-matches("^nulos$|^noregistrados$|^dummy_|^sec_dummy_|^panpanal$|^pripvemprv$|^prdptpc$|^pri$|^pvem$|^prv$", 
                  ignore.case = TRUE))
=======
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
df_main <- read_dta("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/Ayu_Seccion_2013_No_LN.dta")

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

# Merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
df_all <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/all_months_years.dta")

df_all_sub <- df_all %>%
  select(state, section, month, year, lista) %>%
  filter(month== "June", year==2013, state == "VERACRUZ")


# We'll do left_join => 
df_merged <- df_main %>%
  left_join(df_all_sub, by=c("section"))

# drop if _merge==2 => in R means dropping rows with no match => check if 'lista' is NA
df_merged <- df_merged %>%
  filter(!is.na(lista))

# drop _merge, ed, seccion, year, month
df_merged <- df_merged %>%
  select(-any_of(c("state","year","month")))

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
## 2017 (SALVADOR) - Elections held 2016-2017
################################################################################

# Read all sheets from Ayuntamientos_2016.xlsx and Ayuntamientos_2016b.xlsx
# First file - main results
sheets_2016 <- excel_sheets("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/Ayuntamientos_2016.xlsx")

# List of sheets to skip (numbered gaps in SALVADOR file)
sheet_nums_main <- c(1:8, 10:162, 164:169, 171:178)
sheet_names_main <- paste0("Resultados", c("", paste0(" (", 2:178, ")")))
sheet_names_main <- sheet_names_main[sheet_nums_main]
sheet_names_main <- sheet_names_main[sheet_names_main %in% sheets_2016]

df_2017_list_main <- lapply(sheets_2016, function(sheet) {
  tryCatch({
    df <- read_excel("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/Ayuntamientos_2016.xlsx", sheet = sheet, col_types = "text")
    names(df) <- tolower(names(df))
    
    # Replace empty with 0 FIRST
    df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
    
    # THEN handle municipality
    if ("municipality" %in% names(df)) {
      mun_value <- df$municipality[1]
      if (mun_value == "0") mun_value <- NA_character_
      df$municipality <- mun_value
      df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL" & seccion != "0")
    } else {
      df$municipality <- NA_character_
    }
    
    df
  }, error = function(e) NULL)
})

df_2017_main <- bind_rows(df_2017_list_main)


# Second file - additional municipalities (Ayuntamientos_2016b.xlsx)
if (file.exists("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/AyuntamientosB/Ayuntamientos_2016b.xlsx")) {
  sheets_2016b <- excel_sheets("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/AyuntamientosB/Ayuntamientos_2016b.xlsx")
  
  df_2017_list_b <- lapply(sheets_2016b, function(sheet) {
    tryCatch({
      df <- read_excel("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/Other/AyuntamientosB/Ayuntamientos_2016b.xlsx", sheet = sheet, col_types = "text")
      names(df) <- tolower(names(df))
      if ("municipality" %in% names(df)) {
        df$municipality <- df$municipality[1]
        df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL")
      }
      df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
      df
    }, error = function(e) NULL)
  })
  
  df_2017_b <- bind_rows(df_2017_list_b) %>%
    filter(!municipality %in% c("Jaltipan", "Zontecomatlán"))
  
  df_2017 <- bind_rows(df_2017_main, df_2017_b)
} else {
  df_2017 <- df_2017_main
}

# Clean municipality name
df_2017 <- df_2017 %>%
  mutate(municipality = gsub("Reporte de Casillas del municipio de ", "", municipality))

# Drop columns O, P if they exist
df_2017 <- df_2017 %>%
  select(-matches("^o$|^p$", ignore.case = TRUE))

# Rename section
df_2017 <- df_2017 %>%
  rename(section = seccion)

# Convert to numeric
df_2017 <- df_2017 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Handle coalition variants with different spellings/typos
# PAN_PRD coalition - sum all variants
df_2017 <- df_2017 %>%
  mutate(
    PAN_PRD = rowSums(select(., matches("coalicion panprd|coalición panprd|coalicion pan-prd", ignore.case = TRUE)), na.rm = TRUE),
    PAN = ifelse("pan" %in% tolower(names(.)), pan, 0),
    PRD = ifelse("prd" %in% tolower(names(.)), prd, 0)
  ) %>%
  select(-matches("coalicion panprd|coalición panprd|coalicion pan-prd", ignore.case = TRUE))

# PRI_PVEM coalition
df_2017 <- df_2017 %>%
  mutate(
    PRI_PVEM = rowSums(select(., matches("coalicion priverde|coalición priverde", ignore.case = TRUE)), na.rm = TRUE),
    PRI = ifelse("pri" %in% tolower(names(.)), pri, 0),
    PVEM = rowSums(select(., matches("^verde$|^pvem$", ignore.case = TRUE)), na.rm = TRUE)
  ) %>%
  select(-matches("coalicion priverde|coalición priverde", ignore.case = TRUE))

# Standardize party name variants
df_2017 <- df_2017 %>%
  mutate(
    PANAL = rowSums(select(., matches("nueva alianza|nuevaalianza", ignore.case = TRUE)), na.rm = TRUE),
    MC = rowSums(select(., matches("movimiento ciudadano|movimiento cuidadano", ignore.case = TRUE)), na.rm = TRUE),
    PT = rowSums(select(., matches("^pt$", ignore.case = TRUE)), na.rm = TRUE),
    PES = rowSums(select(., matches("encuentro social", ignore.case = TRUE)), na.rm = TRUE),
    MORENA = rowSums(select(., matches("^morena$", ignore.case = TRUE)), na.rm = TRUE)
  )

# Independent candidates (CI_1, CI_2, CI_3)
ci_cols <- names(df_2017)[grepl("Perez|Juarez|Marin|Martinez|Dionisio|Becerra|Pimentel|Andrade|Hernandez|Penaloza|Chazaro|Archer", 
                                names(df_2017), ignore.case = TRUE)]
if (length(ci_cols) > 0) {
  df_2017 <- df_2017 %>%
    mutate(CI_1 = rowSums(select(., all_of(ci_cols)), na.rm = TRUE))
}

# Collapse by municipality and section
df_2017 <- df_2017 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Handle nulos columns
nulo_cols <- names(df_2017)[grepl("Q|VotosNulos", names(df_2017), ignore.case = TRUE)]
df_2017 <- df_2017 %>%
  mutate(nulo = rowSums(select(., any_of(nulo_cols)), na.rm = TRUE))

# Calculate valid and total
party_cols_2017 <- c("PAN", "PRI", "PRD", "PVEM", "PT", "PANAL", "MC", "MORENA", "PES", 
                     "PRI_PVEM", "PAN_PRD", "CI_1", "CI_2", "CI_3")
party_cols_2017 <- party_cols_2017[party_cols_2017 %in% names(df_2017)]

df_2017 <- df_2017 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2017)), na.rm = TRUE),
         total = valid + nulo)

# Read uniqueids mapping
uniqueids_2017 <- read_excel("../../../Data/Raw Electoral Data/Veracruz 2000, 2004, 2007, 2010, 2013,2016,2021,2025/uniqueids.xlsx") %>%
  rename_all(tolower) %>%
  select(municipality, uniqueid, mun) %>%
  distinct()

df_2017 <- df_2017 %>%
  left_join(uniqueids_2017, by = "municipality") %>%
  mutate(municipality = ifelse(!is.na(mun), mun, municipality)) %>%
  select(-mun)

# Assign uniqueid for any missing
df_2017 <- df_2017 %>%
  mutate(municipality = toupper(remove_accents(municipality)),
         uniqueid = ifelse(is.na(uniqueid), assign_veracruz_uniqueid(municipality), uniqueid),
         section = as.numeric(section))

# Filter out missing sections
df_2017 <- df_2017 %>%
  filter(!is.na(section))

# Merge lista nominal 2017
ln_2017 <- read_dta("../../../Data/Raw Electoral Data/Listas Nominales/LN 2012-2019/2017/LN2017.dta") %>%
  filter(entidad == 30, month == 5) %>%
  mutate(uniqueid = entidad * 1000 + municipio,
         seccion = as.numeric(seccion)) %>%
  filter(seccion != 0) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2017 <- df_2017 %>%
  left_join(ln_2017, by = c("uniqueid", "section"))

# Municipal aggregates
df_2017 <- df_2017 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup() %>%
  mutate(turnout = total / listanominal)

df_2017 <- df_2017 %>%
  mutate(year = 2017,
         month = "June",
         STATE = "VERACRUZ")

cat("2017 processed:", nrow(df_2017), "rows\n")
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Assign uniqueid
df_2010 <- df_2010 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2010 <- c("PAN", "PANAL", "PRD", "PT", "PC", "PAN_PANAL", "PRI_PVEM_PRV", "PRD_PT_PC")
party_cols_2010 <- party_cols_2010[party_cols_2010 %in% names(df_2010)]

df_2010 <- df_2010 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2010)), na.rm = TRUE))

<<<<<<< HEAD
# Municipal aggregates
df_2010 <- df_2010 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
=======
# rename SECCION->section, MUNICIPIO->municipality, LISTA_NOMINAL->listanominal, TOTAL->total
df_2018 <- df_2018 %>%
  rename(
    section     = SECCION,
    municipality= MUNICIPIO,
    listanominal= LISTA_NOMINAL,
    total       = TOTAL
  ) %>% 
  rename_with(~ gsub("-", "", .))

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
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33
  ungroup()

df_2010 <- df_2010 %>%
  mutate(year = 2010,
         month = "July")

cat("2010 processed:", nrow(df_2010), "rows\n")

################################################################################
## 2013
################################################################################

df_2013 <- read_dta("Ayu_Seccion_2013_No_LN.dta")

# Rename columns
df_2013 <- df_2013 %>%
  rename(PC = MC,
         PartCardenista = PFCRN)

<<<<<<< HEAD
# Drop nulos and noregistrados
df_2013 <- df_2013 %>%
  select(-matches("NoRegistrados|Nulos"))
=======
# Assign uniqueids
data_2021 <- data_2021 %>% 
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
      municipality == "ALTO LUCERO DE GUTIERREZ BARRIOS"           ~ 30009,
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
      municipality == "CITLALTEPETL"           ~ 30035,
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
      municipality == "COSAUTLAN DE CARVAJAL" ~ 30046,
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
      municipality == "HUILOAPAN DE CUAUHTEMOC"             ~ 30074,
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
      municipality == "LAS VIGAS DE RAMIREZ"             ~ 30132,
      municipality == "LERDO DE TEJADA"       ~ 30097,
      municipality == "LOS REYES"             ~ 30137,
      municipality == "MAGDALENA"             ~ 30098,
      municipality == "MALTRATA"              ~ 30099,
      municipality == "MANLIO FABIO ALTAMIRANO" ~ 30100,
      municipality == "MARIANO ESCOBEDO"      ~ 30101,
      municipality == "MARTINEZ DE LA TORRE"  ~ 30102,
      municipality == "MECATLAN"              ~ 30103,
      municipality == "MECAYAPAN"             ~ 30104,
      municipality == "MEDELLIN DE BRAVO"             ~ 30105,
      municipality == "MIAHUATLAN"            ~ 30106,
      municipality == "MINATITLAN"            ~ 30108,
      municipality == "MISANTLA"             ~ 30109,
      municipality == "MIXTLA DE ALTAMIRANO"  ~ 30110,
      municipality == "MOLOACAN"              ~ 30111,
      municipality == "NANCHITAL DE LAZARO CARDENAS DEL RIO" ~ 30206,
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
      municipality == "POZA RICA DE HIDALGO"             ~ 30131,
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
      municipality == "TATAHUICAPAN DE JUAREZ"          ~ 30209,
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
      municipality == "ZOZOCOLCO DE HIDALGO"             ~ 30203,
      TRUE                                    ~ NA
    )
  )
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Assign uniqueid
df_2013 <- df_2013 %>%
  mutate(municipality = toupper(municipality),
         municipality = remove_accents(municipality),
         uniqueid = assign_veracruz_uniqueid(municipality))

# Calculate valid
party_cols_2013 <- c("PAN", "PRI_PVEM_PANAL", "PRD", "PT", "PC", "AVE", "PartCardenista")
party_cols_2013 <- party_cols_2013[party_cols_2013 %in% names(df_2013)]

df_2013 <- df_2013 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2013)), na.rm = TRUE))

# Merge lista nominal from all_months_years
all_months_2013 <- read_dta("../../all_months_years.dta") %>%
  filter(ed == 30, month == 6, year == 2013) %>%
  select(seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

df_2013 <- df_2013 %>%
  left_join(all_months_2013, by = "section")

df_2013 <- df_2013 %>%
  mutate(turnout = total / listanominal)

# Municipal aggregates
df_2013 <- df_2013 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2013 <- df_2013 %>%
  mutate(year = 2013,
         month = "July")

cat("2013 processed:", nrow(df_2013), "rows\n")

################################################################################
## 2017 (SALVADOR) - Elections held 2016-2017
################################################################################

# Read all sheets from Ayuntamientos_2016.xlsx and Ayuntamientos_2016b.xlsx
# First file - main results
sheets_2016 <- excel_sheets("Ayuntamientos_2016.xlsx")

# List of sheets to skip (numbered gaps in SALVADOR file)
sheet_nums_main <- c(1:8, 10:162, 164:169, 171:178)
sheet_names_main <- paste0("Resultados", c("", paste0(" (", 2:178, ")")))
sheet_names_main <- sheet_names_main[sheet_nums_main]
sheet_names_main <- sheet_names_main[sheet_names_main %in% sheets_2016]

df_2017_list_main <- lapply(sheets_2016, function(sheet) {
  tryCatch({
    df <- read_excel("Ayuntamientos_2016.xlsx", sheet = sheet, col_types = "text")
    names(df) <- tolower(names(df))
    # Fill municipality from first row
    if ("municipality" %in% names(df)) {
      df$municipality <- df$municipality[1]
      df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL")
    }
    # Replace empty with 0
    df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
    df
  }, error = function(e) NULL)
})

df_2017_main <- bind_rows(df_2017_list_main)

# Second file - additional municipalities (Ayuntamientos_2016b.xlsx)
if (file.exists("AyuntamientosB/Ayuntamientos_2016b.xlsx")) {
  sheets_2016b <- excel_sheets("AyuntamientosB/Ayuntamientos_2016b.xlsx")
  
  df_2017_list_b <- lapply(sheets_2016b, function(sheet) {
    tryCatch({
      df <- read_excel("AyuntamientosB/Ayuntamientos_2016b.xlsx", sheet = sheet, col_types = "text")
      names(df) <- tolower(names(df))
      if ("municipality" %in% names(df)) {
        df$municipality <- df$municipality[1]
        df <- df %>% filter(!is.na(seccion) & seccion != "" & seccion != "TOTAL")
      }
      df <- df %>% mutate(across(everything(), ~ifelse(is.na(.) | . == "", "0", .)))
      df
    }, error = function(e) NULL)
  })
  
  df_2017_b <- bind_rows(df_2017_list_b) %>%
    filter(!municipality %in% c("Jaltipan", "Zontecomatlán"))
  
  df_2017 <- bind_rows(df_2017_main, df_2017_b)
} else {
  df_2017 <- df_2017_main
}

# Clean municipality name
df_2017 <- df_2017 %>%
  mutate(municipality = gsub("Reporte de Casillas del municipio de ", "", municipality))

# Drop columns O, P if they exist
df_2017 <- df_2017 %>%
  select(-matches("^o$|^p$", ignore.case = TRUE))

# Rename section
df_2017 <- df_2017 %>%
  rename(section = seccion)

# Convert to numeric
df_2017 <- df_2017 %>%
  mutate(across(-c(municipality, section), ~as.numeric(as.character(.))))

# Handle coalition variants with different spellings/typos
# PAN_PRD coalition
df_2017 <- df_2017 %>%
  mutate(across(matches("CoalicionPANPRD|CoaliciónPANPRD", ignore.case = TRUE),
                ~. + ifelse(!is.na(PAN), PAN, 0) + ifelse(!is.na(PRD), PRD, 0), .names = "temp_panprd"))

if ("temp_panprd" %in% names(df_2017)) {
  df_2017 <- df_2017 %>%
    mutate(PAN_PRD = temp_panprd) %>%
    select(-temp_panprd)
}

# PRI_PVEM coalition
df_2017 <- df_2017 %>%
  mutate(across(matches("CoalicionPRIVERDE|CoalicionPriverde", ignore.case = TRUE),
                ~. + ifelse(!is.na(PRI), PRI, 0) + ifelse(!is.na(VERDE), VERDE, 0) + 
                  ifelse(!is.na(PVEM), PVEM, 0), .names = "temp_priverde"))

if ("temp_priverde" %in% names(df_2017)) {
  df_2017 <- df_2017 %>%
    mutate(PRI_PVEM = temp_priverde) %>%
    select(-temp_priverde, -matches("CoalicionPRIVERDE|CoalicionPriverde", ignore.case = TRUE))
}

# Standardize party name variants
# PANAL (multiple spellings)
df_2017 <- df_2017 %>%
  mutate(PANAL = rowSums(select(., matches("NuevaAlianza|NuevaALianza|Nuevaalianza", ignore.case = TRUE)), na.rm = TRUE))

# MC (typo variants)
df_2017 <- df_2017 %>%
  mutate(MC = rowSums(select(., matches("MovimientoCiudadano|MovimientoCuidadano", ignore.case = TRUE)), na.rm = TRUE))

# PT (case variants)
df_2017 <- df_2017 %>%
  mutate(PT = rowSums(select(., matches("^PT$|^Pt$|^pt$")), na.rm = TRUE))

# PES
df_2017 <- df_2017 %>%
  rename_with(~"PES", matches("EncuentroSocial"))

# MORENA
df_2017 <- df_2017 %>%
  mutate(MORENA = rowSums(select(., matches("^MORENA$|^Morena$", ignore.case = TRUE)), na.rm = TRUE))

# Independent candidates (CI_1, CI_2, CI_3)
ci_cols <- names(df_2017)[grepl("Perez|Juarez|Marin|Martinez|Dionisio|Becerra|Pimentel|Andrade|Hernandez|Penaloza|Chazaro|Archer", 
                                 names(df_2017), ignore.case = TRUE)]
if (length(ci_cols) > 0) {
  df_2017 <- df_2017 %>%
    mutate(CI_1 = rowSums(select(., all_of(ci_cols)), na.rm = TRUE))
}

# Collapse by municipality and section
df_2017 <- df_2017 %>%
  group_by(municipality, section) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

# Handle nulos columns
nulo_cols <- names(df_2017)[grepl("Q|VotosNulos", names(df_2017), ignore.case = TRUE)]
df_2017 <- df_2017 %>%
  mutate(nulo = rowSums(select(., any_of(nulo_cols)), na.rm = TRUE))

# Calculate valid and total
party_cols_2017 <- c("PAN", "PRI", "PRD", "PVEM", "PT", "PANAL", "MC", "MORENA", "PES", 
                     "PRI_PVEM", "PAN_PRD", "CI_1", "CI_2", "CI_3")
party_cols_2017 <- party_cols_2017[party_cols_2017 %in% names(df_2017)]

df_2017 <- df_2017 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2017)), na.rm = TRUE),
         total = valid + nulo)

# Read uniqueids mapping
uniqueids_2017 <- read_excel("uniqueids.xlsx") %>%
  rename_all(tolower) %>%
  select(municipality, uniqueid, MUN) %>%
  distinct()

df_2017 <- df_2017 %>%
  left_join(uniqueids_2017, by = "municipality") %>%
  mutate(municipality = ifelse(!is.na(MUN), MUN, municipality)) %>%
  select(-MUN)

# Assign uniqueid for any missing
df_2017 <- df_2017 %>%
  mutate(municipality = toupper(remove_accents(municipality)),
         uniqueid = ifelse(is.na(uniqueid), assign_veracruz_uniqueid(municipality), uniqueid))

# Filter out missing sections
df_2017 <- df_2017 %>%
  filter(!is.na(section))

# Merge lista nominal 2017
ln_2017 <- read_dta("../Listas Nominales/LN 2012-2019/2017/LN2017.dta") %>%
  filter(entidad == 30, month == 5) %>%
  mutate(uniqueid = entidad * 1000 + municipio) %>%
  filter(seccion != 0) %>%
  select(uniqueid, seccion, lista) %>%
  rename(section = seccion, listanominal = lista)

<<<<<<< HEAD
df_2017 <- df_2017 %>%
  left_join(ln_2017, by = c("uniqueid", "section"))
=======
# Check and process coalitions
magar_coal <- read_csv("../../../Data/new magar data splitcoal/aymu1988-on-v7-coalSplit.csv") %>% 
  filter(yr >= 2020 & edon == 30) %>% 
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
collapsed_2021 <- process_coalitions(collapsed_2021, magar_coal) %>% 
  select(-coal1, -coal2, -coal3, -coal4)

#####################################
### PROCESSING DATA FOR 2025 -------
#####################################
library(purrr)
library(tibble)
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

# Municipal aggregates
df_2017 <- df_2017 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup() %>%
  mutate(turnout = total / listanominal)

df_2017 <- df_2017 %>%
  mutate(year = 2017,
         month = "June",
         STATE = "VERACRUZ")

cat("2017 processed:", nrow(df_2017), "rows\n")

################################################################################
## 2018 Extraordinary (SALVADOR)
## Municipalities: CAMARON DE TEJEDA, EMILIANO ZAPATA, SAYULA DE ALEMAN
################################################################################

df_2018 <- read_excel("VERACRUZ_EXTRAORDINARIAMPAL_2018.xlsx", 
                       sheet = "VERACRUZ_EXTRAORDINARIAMPAL_201",
                       range = "A7:AM138")

df_2018 <- df_2018 %>%
  rename(section = SECCION,
         municipality = MUNICIPIO,
         listanominal = LISTA_NOMINAL,
         total = TOTAL)

# Create coalitions
df_2018 <- df_2018 %>%
  mutate(PAN_PRD = PAN + PRD + PANPRD,
         PRI_PVEM = PRI + PVEM + PRIPVEM)

# Remove original party columns absorbed into coalitions
df_2018 <- df_2018 %>%
  select(-PAN, -PRD, -PANPRD, -PRI, -PVEM, -PRIPVEM)

# MORENA_PT_PES coalition for EMILIANO ZAPATA
df_2018 <- df_2018 %>%
  mutate(MORENA_PT_PES = ifelse(municipality == "EMILIANO ZAPATA",
                                MORENA + PES + PT + PTMORENAPES + PTPES + PTMORENA + MORENAPES, NA))

# Remove absorbed columns
df_2018 <- df_2018 %>%
  select(-PT, -MORENA, -PTMORENAPES, -PTPES, -PTMORENA, -MORENAPES)

# Set PES to NA for EMILIANO ZAPATA
df_2018 <- df_2018 %>%
  mutate(PES = ifelse(municipality == "EMILIANO ZAPATA", NA, PES))

# Collapse
df_2018 <- df_2018 %>%
  group_by(municipality, section) %>%
  summarise(across(c(PAN_PRD, PRI_PVEM, MORENA_PT_PES, PANAL, PES, listanominal, total), 
                   ~sum(., na.rm = TRUE)), .groups = "drop")

# Assign uniqueid
df_2018 <- df_2018 %>%
  mutate(uniqueid = case_when(
    municipality == "CAMARON DE TEJEDA" ~ 30007,
    municipality == "EMILIANO ZAPATA" ~ 30065,
    municipality == "SAYULA DE ALEMAN" ~ 30144,
    TRUE ~ NA_real_
  ))

# Mark as extraordinary
df_2018 <- df_2018 %>%
  mutate(municipality = paste0(municipality, " EXTRAORDINARIO"))

df_2018 <- df_2018 %>%
  mutate(turnout = total / listanominal)

# Calculate valid
party_cols_2018 <- c("PAN_PRD", "PRI_PVEM", "MORENA_PT_PES", "PANAL", "PES")
df_2018 <- df_2018 %>%
  mutate(valid = rowSums(select(., all_of(party_cols_2018)), na.rm = TRUE))

# Municipal aggregates
df_2018 <- df_2018 %>%
  group_by(uniqueid) %>%
  mutate(mun_total = sum(total, na.rm = TRUE),
         mun_valid = sum(valid, na.rm = TRUE),
         mun_listanominal = sum(listanominal, na.rm = TRUE),
         mun_turnout = mun_total / mun_listanominal) %>%
  ungroup()

df_2018 <- df_2018 %>%
  mutate(year = 2018,
         month = "March")

cat("2018 Extraordinary processed:", nrow(df_2018), "rows\n")

################################################################################
## Append All Years
################################################################################

all_years <- list(df_2004, df_2005, df_2007, df_2010, df_2013, df_2017, df_2018)

# Get all unique column names
all_cols <- unique(unlist(lapply(all_years, names)))

# Ensure all dataframes have all columns
standardize_df <- function(df, all_cols) {
  for (col in all_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

all_years <- lapply(all_years, standardize_df, all_cols)

<<<<<<< HEAD
# Combine all years
veracruz_all <- bind_rows(all_years)

################################################################################
## Final Corrections
################################################################################

veracruz_all <- veracruz_all %>%
  mutate(municipality = toupper(remove_accents(municipality)))

# Final uniqueid assignment for any missing
veracruz_all <- veracruz_all %>%
  mutate(uniqueid = ifelse(is.na(uniqueid), assign_veracruz_uniqueid(municipality), uniqueid))
=======
# Rename columns
data_2025 <- data_2025_cons %>%
  dplyr::rename(municipality = municipio,
                section = seccion,
                listanominal = listaNominal,
                total = sumaTotal,
                no_reg = CNR,
                nulos = VN,
                PAN               = pan,
                PRI               = pri,
                PVEM              = verde,
                PT                = pt,
                MC                = mc,
                MORENA            = morena,
                no_reg            = CNR,
                nulos             = VN,
                total             = sumaTotal,
                uniqueid          = municipio_id,
                sheet_name        = sheet_name,
                PVEM_MORENA       = `verde-morena`,
                CI_1  = nlr,
                CI_2  = mall,
                CI_3  = ahr,
                CI_4  = rrg,
                CI_5  = vgh,
                CI_6  = apm,
                CI_7  = fcl,
                CI_8  = lcc,
                CI_9  = ydlacz,
                CI_10 = lygt,
                CI_11 = sht,
                CI_12 = jcr,
                CI_13 = sst,
                CI_14 = magm,
                CI_15 = jcmc) %>%
  dplyr::mutate(
    section = as.numeric(section),
    uniqueid =  30000 + municipality
  ) %>% 
  dplyr::filter(section > 0)

# Group by section, and uniqueid, and sum the relevant columns
collapsed_2025 <- data_2025 %>%
  dplyr::group_by(section, uniqueid) %>%
  dplyr::summarise(
    across(c(listanominal, PAN:total, PVEM_MORENA:CI_15), 
           \(x) sum(x, na.rm = TRUE))
  )

# Calculate valid votes and final details
collapsed_2025 <- collapsed_2025 %>%
  dplyr::mutate(
    turnout = total/listanominal,
    valid = sum(c_across(c(PAN:MORENA, PVEM_MORENA:CI_15)), na.rm = TRUE),
    year = 2025,
    month = "June"
  ) 

# Process Coalitions
collapsed_2025 <- collapsed_2025 %>%
  mutate(
    PVEM = PVEM + ifelse(PVEM_MORENA > 0, PVEM_MORENA, 0),
    MORENA = MORENA + ifelse(PVEM_MORENA > 0, PVEM_MORENA, 0),
    PVEM_MORENA = ifelse(PVEM_MORENA > 0, 0, PVEM_MORENA)
  )
>>>>>>> a21cdeff03c45281c2c187a1d1995c0e6006ce33

################################################################################
## Save Output
################################################################################

# Reorder columns
key_cols <- c("municipality", "uniqueid", "section", "year", "month")
other_cols <- setdiff(names(veracruz_all), key_cols)
veracruz_all <- veracruz_all %>%
  select(all_of(key_cols), all_of(other_cols))

# Save
data.table::fwrite(veracruz_all, "../../../Processed Data/veracruz/veracruz_process_raw_data.csv")

cat("\n=== VERACRUZ PROCESSING COMPLETE ===\n")
cat("Total observations:", nrow(veracruz_all), "\n")
cat("Years:", paste(sort(unique(veracruz_all$year)), collapse = ", "), "\n")
cat("Municipalities:", length(unique(veracruz_all$uniqueid)), "\n")
