
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Guerrero - 1996, 1999, 2002, 2005, 2008, 2012"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1996_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

destring pan -  nulos, replace

egen total = rowtotal(pan pri pps prd pc prt pvem pt cc1 validos nulos)

drop if total==.| total==0

collapse (sum) pan -  nulos total, by (municipality section coalition)

*************************************************************************************************

gen pps_pc = 0
replace pps_pc = pps + pc + cc1 if coalition=="PPS,PC"
replace pps = 0 if coalition=="PPS,PC"
replace pc  = 0 if coalition=="PPS,PC"
replace cc1 = 0 if coalition=="PPS,PC"

gen pps_prd_pc = 0
replace pps_prd_pc = pps +prd + pc + cc1 if coalition=="PPS,PRD,PC"
replace pps = 0 if coalition=="PPS,PRD,PC"
replace prd = 0 if coalition=="PPS,PRD,PC"
replace pc  = 0 if coalition=="PPS,PRD,PC"
replace cc1 = 0 if coalition=="PPS,PRD,PC"

gen prd_pc = 0
replace prd_pc = prd + pc + cc1 if coalition=="PRD,PC"
replace prd = 0 if coalition=="PRD,PC"
replace pc  = 0 if coalition=="PRD,PC"
replace cc1 = 0 if coalition=="PRD,PC"

gen prd_pc_prt = 0
replace prd_pc_prt = prd + pc + prt + cc1 if coalition=="PRD,PC,PRT"
replace prd = 0 if coalition=="PRD,PC,PRT"
replace pc  = 0 if coalition=="PRD,PC,PRT"
replace prt  = 0 if coalition=="PRD,PC,PRT"
replace cc1 = 0 if coalition=="PRD,PC,PRT"

gen prd_pc_pvem_pt = 0
replace prd_pc_pvem_pt = prd + pc + pvem + pt + cc1 if coalition=="PRD,PC,PVEM,PT"
replace prd = 0 if coalition=="PRD,PC,PVEM,PT"
replace pc  = 0 if coalition=="PRD,PC,PVEM,PT"
replace pvem  = 0 if coalition=="PRD,PC,PVEM,PT"
replace pt  = 0 if coalition=="PRD,PC,PVEM,PT"
replace cc1 = 0 if coalition=="PRD,PC,PVEM,PT"

sum cc1
drop cc1 

*************************************************************************************************

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     Part_Card
rename  pt     PT
rename  pvem   PVEM
rename  pps    PPS
rename  prt    PRT
rename  pps_pc          Part_Card_PPS
rename  pps_prd_pc      PRD_Part_Card_PPS
rename  prd_pc          PRD_Part_Card
rename  prd_pc_prt      PRD_Part_Card_PRT
rename  prd_pc_pvem_pt  PRD_Part_Card_PVEM_PT

* gen turnout =  total/nominal

drop  validos  nulos 

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MA IZAZAGA"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GENERAL HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12068 if municipality =="UNION DE ISIDORO MONTES DE OCA, LA"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPA"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if municipality =="PETATLAN"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="JOSE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO"
replace uniqueid=12074 if municipality =="ZITLALA"

egen valid = rowtotal(PAN PRI PPS PRD Part_Card PRT PVEM PT Part_Card_PPS PRD_Part_Card_PPS PRD_Part_Card PRD_Part_Card_PRT PRD_Part_Card_PVEM_PT)

foreach var in PAN PRI PPS PRD Part_Card PRT PVEM PT Part_Card_PPS PRD_Part_Card_PPS PRD_Part_Card PRD_Part_Card_PRT PRD_Part_Card_PVEM_PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PPS inv_mun_PRD inv_mun_Part_Card inv_mun_PRT inv_mun_PVEM inv_mun_PT inv_mun_Part_Card_PPS inv_mun_PRD_Part_Card_PPS inv_mun_PRD_Part_Card inv_mun_PRD_Part_Card_PRT inv_mun_PRD_Part_Card_PVEM_PT, gen(PAN_r PRI_r PPS_r PRD_r Part_Card_r PRT_r PVEM_r PT_r Part_Card_PPS_r PRD_Part_Card_PPS_r PRD_Part_Card_r PRD_Part_Card_PRT_r PRD_Part_Card_PVEM_PT_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PVEM" if PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PRT" if PRT_r==1
replace winner = "Partido_Cardenista" if Part_Card_r==1
replace winner = "Partido_Cardenista_PPS" if Part_Card_PPS_r==1
replace winner = "PRD_Partido_Cardenista_PPS" if PRD_Part_Card_PPS_r==1
replace winner = "PRD_Partido_Cardenista" if PRD_Part_Card_r==1
replace winner = "PRD_Partido_Cardenista_PRT" if PRD_Part_Card_PRT_r==1
replace winner = "PRD_Partido_Cardenista_PVEM_PT" if PRD_Part_Card_PVEM_PT_r==1

drop *_r

gen year =1996
gen month ="October"

save Guerrero_Section_1996.dta, replace


*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1999_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

destring pan -  total, replace

drop if total==.| total==0

collapse (sum) pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM
rename  prs    PRS

* gen turnout =  total/nominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GRAL. HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12068 if municipality =="LA UNION"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if municipality =="PETATLAN"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="JOSE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO DE LOS CHAVEZ"
replace uniqueid=12074 if municipality =="ZITLALA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PRS)

foreach var in PAN PRI PRD PT PVEM PRS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PRS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PRS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PVEM" if PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PRS" if PRS_r==1
drop *_r

gen year =1999
gen month ="October"

save Guerrero_Section_1999.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "INE-CI141-2014 Horacio Larreguy Arbesu\pdln12_edms_PEL_2002_2005.xls", sheet("pdln12_edms") firstrow clear
keep if FECHA=="20021006"

rename SEC section
rename LISTA listanominal

collapse (sum) listanominal, by(section)

save Listanominal2002.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2002_No_LN.csv, clear

rename nombre_municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan -  total, replace

collapse (sum) pan -  prdpt total, by (municipality section)

rename  pan    PAN
rename  pripvem    PRI_PVEM
rename  prd    PRD
rename  pt     PT
rename  prs    PRS
rename  pc     PC
rename  psn    PSN
rename  pas    PAS
rename  psm    PSM
rename  prdpt    PRD_PT

drop if municipality=="ACATEPEC" & section==2527

merge 1:1 section using Listanominal2002.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GRAL. CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GRAL. HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12068 if municipality =="LA UNION"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if municipality =="PETATLAN"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE M."
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="JOSE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO DE LOS CHAVEZ"
replace uniqueid=12074 if municipality =="ZITLALA"

egen valid = rowtotal(PAN PRI_PVEM PRD PT PRS PC PSN PAS PSM PRD_PT)

foreach var in PAN PRI_PVEM PRD PT PRS PC PSN PAS PSM PRD_PT listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PRS inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PSM inv_mun_PRD_PT, gen(PAN_r PRI_PVEM_r PRD_r PT_r PRS_r PC_r PSN_r PAS_r PSM_r PRD_PT_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PRS" if PRS_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PSM" if PSM_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
drop *_r

gen year =2002
gen month ="October"

sort section

save Guerrero_Section_2002.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "INE-CI141-2014 Horacio Larreguy Arbesu\pdln12_edms_PEL_2002_2005.xls", sheet("pdln12_edms") firstrow clear
keep if FECHA=="20051002"

rename SEC section
rename LISTA listanominal

collapse (sum) listanominal, by(section)

save Listanominal2005.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2005_No_LN.csv, clear

rename nombre_municipio  municipality
rename seccion section
* rename lista_nominal listanominal
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan - prdprs total, replace

collapse (sum) pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM
rename  prs    PRS
rename  pc     PC
rename  prdprs PRD_PRS

merge 1:1 section using Listanominal2005.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop nulos

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINTO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA  DE J. M. I."
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GRAL. CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GRAL. HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE C."
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12068 if municipality =="LA UNION DE ISIDORO M. O."
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12077 if municipality =="MARQUELIA"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO A."
replace uniqueid=12048 if municipality =="PETATLAN"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="JOSE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO DE LOS CHAVEZ"
replace uniqueid=12074 if municipality =="ZITLALA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PRS PC PRD_PRS)

foreach var in PAN PRI PRD PT PVEM PRS PC PRD_PRS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PRS inv_mun_PC inv_mun_PRD_PRS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PRS_r PC_r PRD_PRS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PRS" if PRS_r==1
replace winner = "PC" if PC_r==1
replace winner = "PRD_PRS" if PRD_PRS_r==1
drop *_r

gen year =2005
gen month ="October"

sort section

save Guerrero_Section_2005.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2008.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal pan -  total, replace

collapse (sum) listanominal  pan -  ptpc total, by (municipality section)

rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pvem PVEM
rename pc PC
rename panal PANAL
rename pas PAS
rename alianzaguerrero PAG
rename pripvem PRI_PVEM
rename ptpc PT_PC

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MARIA IZAZAGA"
replace uniqueid=12078 if municipality =="COCHOAPA EL GRANDE"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GENERAL HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12081 if municipality =="ILIATENCO"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12079 if municipality =="JOSE JOAQUIN DE HERRERA"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12080 if municipality =="JUCHITAN"
replace uniqueid=12068 if municipality =="LA UNION DE ISIDORO MONTES DE OCA"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12077 if municipality =="MARQUELIA"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if municipality =="PETATLAN"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="ZIHUATANEJO DE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO"
replace uniqueid=12074 if municipality =="ZITLALA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAS PAG PRI_PVEM PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAS PAG PRI_PVEM PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAS inv_mun_PAG inv_mun_PRI_PVEM inv_mun_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAS_r PAG_r PRI_PVEM_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PAG" if PAG_r==1
replace winner = "PRI_PVEM" if PRI_PVEM_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year =2008
gen month ="October"

sort section

save Guerrero_Section_2008.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2012.xlsx", sheet("Sheet1") firstrow clear

rename MUNICIPIO  municipality
rename SECCION  section
rename TOTALES total
drop if municipality=="" | section==.
drop if total==. | total==0

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12006 if municipality =="APAXTLA"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MARIA IZAZAGA"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MA IZAZAGA"
replace uniqueid=12078 if municipality =="COCHOAPA EL GRANDE"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GENERAL HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12081 if municipality =="ILIATENCO"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12079 if municipality =="JOSE JOAQUIN DE HERRERA"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12080 if municipality =="JUCHITAN"
replace uniqueid=12068 if municipality =="LA UNION DE ISIDORO MONTES DE OCA"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12077 if municipality =="MARQUELIA"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if strpos(municipality, "PETATLAN")>0
replace uniqueid=12048 if municipality =="PETATLAN*"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="ZIHUATANEJO DE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO"
replace uniqueid=12074 if municipality =="ZITLALA"

collapse (sum) PAN - PRD_PT_PC   total, by (uniqueid section)

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PRI_PVEM PRD_PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PANAL PRI_PVEM PRD_PT_PC total  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PRI_PVEM inv_mun_PRD_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PRI_PVEM_r PRD_PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PRI_PVEM" if PRI_PVEM_r==1
replace winner = "PRD_PT_PC" if PRD_PT_PC_r==1
drop *_r

gen year =2012
gen month ="July"

sort section

save Guerrero_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Guerrero_Section_1996.dta, clear
append using Guerrero_Section_1999.dta
append using Guerrero_Section_2002.dta
append using Guerrero_Section_2005.dta
append using Guerrero_Section_2008.dta
append using Guerrero_Section_2012.dta

erase Guerrero_Section_1996.dta
erase Guerrero_Section_1999.dta
erase Guerrero_Section_2002.dta
erase Guerrero_Section_2005.dta
erase Guerrero_Section_2008.dta
erase Guerrero_Section_2012.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PRT PRS Partido_Cardenista {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
tab winner winner_counter

rename Part_Card Partido_Cardenista
rename Part_Card_PPS Partido_Cardenista_PPS
rename PRD_Part_Card_PPS PRD_Partido_Cardenista_PPS
rename PRD_Part_Card PRD_Partido_Cardenista
rename PRD_Part_Card_PRT PRD_Partido_Cardenista_PRT
rename PRD_Part_Card_PVEM_PT PRD_Partido_Cardenista_PVEM_PT

save Guerrero_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Guerrero_ALL.dta, replace
