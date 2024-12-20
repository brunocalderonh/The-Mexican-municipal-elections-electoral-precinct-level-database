
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Zacatecas 1998, 2001, 2004, 2007, 2010, 2013"


**************************************************************************
**************************************************************************
**************************************************************************


insheet using Ayu_Seccion_1998_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal
rename  totalemitida total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - total , replace

collapse (sum)  pan - total , by (municipality section)
 
rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pdp  PDP

* gen turnout =  total/listanominal

drop  nulos noreg

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CAÑITAS DE F. PESCADOR"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="GRAL. JOAQUIN AMARO"
replace uniqueid=32041 if municipality =="EL SALVADOR"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GRAL. ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GRAL. FCO. R. MURGUIA"
replace uniqueid=32016 if municipality =="GRAL. PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA"
replace uniqueid=32034 if municipality =="NOCHISTLAN"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZALEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO"
replace uniqueid=32011 if municipality =="GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PAN PRI PRD PT PDP)

foreach var in PAN PRI PRD PT PDP total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PDP, gen(PAN_r PRI_r PRD_r PT_r PDP_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PDP" if PDP_r ==1 
drop *_r

gen year = 1998
gen month ="July"

save Zacatecas_Section_1998.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal
rename   cnr noregistrados
rename   vn nulos
rename   votacionemitida total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - total , replace

collapse (sum)  pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pas  PAS
rename cdppn PC
rename pns PNS

* gen turnout =  total/listanominal

drop noregistrados nulos

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CAÑITAS"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="EL PLATEADO"
replace uniqueid=32041 if municipality =="EL SALVADOR"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GRAL. ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GRAL FCO R MURGUIA"
replace uniqueid=32016 if municipality =="GRAL. PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JMIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA"
replace uniqueid=32034 if municipality =="NOCHISTLAN"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GLEZ. ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PAN PRI PRD PT PVEM PNS PAS PC)

foreach var in PAN PRI PRD PT PVEM PNS PAS PC total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal
rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PNS inv_mun_PAS inv_mun_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PNS_r PAS_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PNS" if PNS_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2001
gen month ="July"

save Zacatecas_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename priptpvem  PRI_PT_PVEM
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc  PC

gen turnout =  total/listanominal

drop  nulos   

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CA?ITAS DE FELIPE PESCADOR"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="PLATEADO DE JOAQUIN AMARO EL"
replace uniqueid=32041 if municipality =="SALVADOR EL"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GENERAL ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GENERAL FRANCISCO R. MURGUIA"
replace uniqueid=32016 if municipality =="GENERAL PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLAN DE MEJIA"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZALEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO DE SANCHEZ ROMAN"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PAN PRI_PT_PVEM PRI PRD PT PVEM PC)

foreach var in PAN PRI_PT_PVEM PRI PRD PT PVEM PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PT_PVEM inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC, gen(PAN_r PRI_PT_PVEM_r PRI_r PRD_r PT_r PVEM_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PT_PVEM" if PRI_PT_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2004
gen month ="July"

save Zacatecas_Section_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prdpc  PRD_PC
rename pt   PT
rename pvem PVEM
rename panal  PANAL
rename pas  PAS

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CA?ITAS DE FELIPE PESCADOR"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="PLATEADO DE JOAQUIN AMARO EL"
replace uniqueid=32041 if municipality =="SALVADOR EL"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GENERAL ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GENERAL FRANCISCO R MURGUIA"
replace uniqueid=32016 if municipality =="GENERAL PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLAN DE MEJIA"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32058 if municipality =="SANTA MARIA DE LA PAZ"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZALEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO DE SANCHEZ ROMAN"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PAN PRI PRD_PC PT PVEM PANAL PAS)

foreach var in PAN PRI PRD_PC PT PVEM PANAL PAS listanominal  total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PC inv_mun_PT inv_mun_PVEM inv_mun_PANAL inv_mun_PAS, gen(PAN_r PRI_r PRD_PC_r PT_r PVEM_r PANAL_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD_PC" if PRD_PC_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PAS" if PAS_r ==1 
drop *_r

gen year = 2007
gen month ="July"

sort section

save Zacatecas_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)

*************************************************************************

gen sec_dummy_panpt = 0
replace sec_dummy_panpt = 1 if panpt>0

bys municipality: egen dummy_panpt = max(sec_dummy_panpt)

gen pan_pt = panpt + pan + pt if dummy_panpt==1
replace pan = 0 if dummy_panpt==1
replace pt = 0 if dummy_panpt==1
drop panpt sec_dummy_panpt dummy_panpt

*************************************************************************

rename pan  PAN
rename pan_pt PAN_PT
rename pripvempanal  PRI_PVEM_PANAL
rename prdpc  PRD_PC
rename pt   PT

gen turnout =  total/listanominal

drop  nulos 

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CA?ITAS DE FELIPE PESCADOR"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="PLATEADO DE JOAQUIN AMARO EL"
replace uniqueid=32041 if municipality =="SALVADOR EL"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GENERAL ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GENERAL FRANCISCO R MURGUIA"
replace uniqueid=32016 if municipality =="GENERAL PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLAN DE MEJIA"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32058 if municipality =="SANTA MARIA DE LA PAZ"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZALEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO DE SANCHEZ ROMAN"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PAN PAN_PT PRI_PVEM_PANAL PRD_PC PT )

foreach var in PAN PAN_PT PRI_PVEM_PANAL PRD_PC PT  listanominal  total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PAN_PT inv_mun_PRI_PVEM_PANAL inv_mun_PRD_PC inv_mun_PT , gen(PAN_r PAN_PT_r PRI_PVEM_PANAL_r PRD_PC_r PT_r )
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PAN_PT" if PAN_PT_r==1  
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRD_PC" if PRD_PC_r ==1 
replace winner = "PT" if PT_r ==1 
drop *_r

gen year = 2010
gen month ="July"

sort section

save Zacatecas_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("CASILLAS_AYUNTAMIENTOS_2013") firstrow clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.

gen PAN_PRD = PAN + PRD + PANPRD 
drop PAN PRD PANPRD
rename MC PC 

egen total = rowtotal(PRI PT PVEM PC PANAL PAN_PRD MinorParty1 MinorParty2 MinorParty3 MinorParty4 MinorParty5 MinorParty6 MinorParty7 MinorParty8 MinorParty9 Nulos)
drop if total==. | total==0 

destring  listanominal PRI - total , replace

collapse (sum)  listanominal PRI - total , by (municipality section)

gen turnout = total / listanominal

drop Nulos

gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUAREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CA+ITAS DE FELIPE PESCADOR"
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCION DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTEMOC"
replace uniqueid=32015 if municipality =="PLATEADO DE JOAQUIN AMARO, EL"
replace uniqueid=32041 if municipality =="SALVADOR, EL"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GENERAL ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GENERAL FRANCISCO R. MURGUIA"
replace uniqueid=32016 if municipality =="GENERAL PANFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMENEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLAN DE MEJIA"
replace uniqueid=32035 if municipality =="NORIA DE ANGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PANUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RIO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32058 if municipality =="SANTA MARIA DE LA PAZ"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACAN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLAN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZALEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO DE SANCHEZ ROMAN"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCIA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCIA"
replace uniqueid=32053 if municipality =="VILLA GONZALEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

egen valid = rowtotal(PRI PT PVEM PC PANAL PAN_PRD MinorParty1 MinorParty2 MinorParty3 MinorParty4 MinorParty5 MinorParty6 MinorParty7 MinorParty8 MinorParty9 )

foreach var in PRI PT PVEM PC PANAL PAN_PRD MinorParty1 MinorParty2 MinorParty3 MinorParty4 MinorParty5 MinorParty6 MinorParty7 MinorParty8 MinorParty9 listanominal  total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAN_PRD inv_mun_MinorParty1 inv_mun_MinorParty2 inv_mun_MinorParty3 inv_mun_MinorParty4 inv_mun_MinorParty5 inv_mun_MinorParty6 inv_mun_MinorParty7 inv_mun_MinorParty8 inv_mun_MinorParty9, gen(PRI_r PT_r PVEM_r PC_r PANAL_r PAN_PRD_r MinorParty1_r MinorParty2_r MinorParty3_r MinorParty4_r MinorParty5_r MinorParty6_r MinorParty7_r MinorParty8_r MinorParty9_r)
drop inv_mun_*

* MinorParty1 MinorParty2 MinorParty3 MinorParty4 MinorParty5 MinorParty6 MinorParty7 MinorParty8 MinorParty9
gen  winner = "PAN_PRD" if PAN_PRD_r ==1 
replace winner = "PRI" if PRI_r==1  
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PANAL" if PANAL_r==1  
replace winner = "CI" if MinorParty1_r==1
drop *_r

gen year = 2013
gen month ="July"

save Zacatecas_Section_2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Zacatecas_Section_1998.dta
append using  Zacatecas_Section_2001.dta
append using  Zacatecas_Section_2004.dta
append using  Zacatecas_Section_2007.dta
append using  Zacatecas_Section_2010.dta
append using  Zacatecas_Section_2013.dta

erase Zacatecas_Section_1998.dta
erase Zacatecas_Section_2001.dta
erase Zacatecas_Section_2004.dta
erase Zacatecas_Section_2007.dta
erase Zacatecas_Section_2010.dta
erase Zacatecas_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC PFC PAC PJS PST PRV PY PAY CI {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Zacatecas_ALL.dta, replace

cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Zacatecas_ALL.dta, replace

* Checked for not missing coalition
