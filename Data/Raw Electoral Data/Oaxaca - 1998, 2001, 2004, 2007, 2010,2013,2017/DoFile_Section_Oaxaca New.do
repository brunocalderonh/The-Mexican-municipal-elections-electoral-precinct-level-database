
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Oaxaca - 1998, 2001, 2004, 2007, 2010 - Missing 2013  (Requested)"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Oaxaca - 1998, 2001, 2004, 2007, 2010 - Missing 2013  (Requested)"


**************************************************************************
**************************************************************************
**************************************************************************

import excel "Concejales 1998.xls", sheet("Votacion por casilla Municipal") cellrange(A2:T2619) firstrow clear

drop if Municipio=="Total del Municipio:"
drop if Cve==""

replace Municipio = Municipio[_n-1] if Cve == Cve[_n-1]

drop if strpos(PAN, "ANULADA")>0 | strpos(PAN, "DESTRUIDA")>0 | strpos(PAN, "INSTALO")>0
drop if strpos(ListaNominal, "INSTALO")>0
destring ListaNominal PAN, replace

rename ListaNominal listanominal

rename Secc section 
destring section, replace
rename Municipio municipality
rename VTotalEmitida total
rename TVotosVlidos valid

drop Cve NoReg Nulos TdeAbst Vot Abst T

collapse (sum) listanominal - total , by(municipality section)

gen turnout =  total/listanominal

replace municipality = subinstr(municipality, "á", "a", .)
replace municipality = subinstr(municipality, "é", "e", .)
replace municipality = subinstr(municipality, "í", "i", .)
replace municipality = subinstr(municipality, "ó", "o", .)

replace municipality = upper(municipality)
gen   uniqueid= 0
replace uniqueid=20002 if municipality =="ACATLAN DE PEREZ FIGUEROA"
replace uniqueid=20004 if municipality =="ASUNCION CUYOTEPEJI"
replace uniqueid=20005 if municipality =="ASUNCION IXTALTEPEC"
replace uniqueid=20006 if municipality =="ASUNCION NOCHIXTLAN"
replace uniqueid=20007 if municipality =="ASUNCION OCOTLAN"
replace uniqueid=20009 if municipality =="AYOTZINTEPEC"
replace uniqueid=20025 if municipality =="CHAHUITES"
replace uniqueid=20026 if municipality =="CHALCATONGO DE HIDALGO"
replace uniqueid=20013 if municipality =="CIENEGA DE ZIMATLAN"
replace uniqueid=20014 if municipality =="CIUDAD IXTEPEC"
replace uniqueid=20021 if municipality =="COSOLAPA"
replace uniqueid=20023 if municipality =="CUILAPAM DE GUERRERO"
replace uniqueid=20028 if municipality =="EJUTLA DE CRESPO"
replace uniqueid=20010 if municipality =="BARRIO DE LA SOLEDAD, EL"
replace uniqueid=20030 if municipality =="EL ESPINAL"
replace uniqueid=20032 if municipality =="FRESNILLO DE TRUJANO"
replace uniqueid=20034 if municipality =="GUADALUPE DE RAMIREZ"
replace uniqueid=20397 if municipality =="H. CIUDAD DE TLAXIACO"
replace uniqueid=20039 if municipality =="HUAJUAPAN DE LEON"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20053 if municipality =="MAGDALENA TLACOTEPEC"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO"
replace uniqueid=20059 if municipality =="MIAHUATLAN DE PORFIRIO DIAZ"
replace uniqueid=20067 if municipality =="OAXACA DE JUAREZ"
replace uniqueid=20068 if municipality =="OCOTLAN DE MORELOS"
replace uniqueid=20070 if municipality =="PINOTEPA DE DON LUIS"
replace uniqueid=20073 if municipality =="PUTLA VILLA DE GUERRERO"
replace uniqueid=20075 if municipality =="REFORMA DE PINEDA"
replace uniqueid=20079 if municipality =="SALINA CRUZ"
replace uniqueid=20080 if municipality =="SAN AGUSTIN AMATENGO"
replace uniqueid=20081 if municipality =="SAN AGUSTIN ATENANGO"
replace uniqueid=20089 if municipality =="SAN ANDRES DINICUITI"
replace uniqueid=20090 if municipality =="SAN ANDRES HUAXPALTEPEC"
replace uniqueid=20102 if municipality =="SAN ANDRES ZAUTLA"
replace uniqueid=20103 if municipality =="SAN ANTONINO CASTILLO VELASCO"
replace uniqueid=20112 if municipality =="SAN BALTAZAR CHICHICAPAM"
replace uniqueid=20116 if municipality =="SAN BARTOLOME AYAUTLA"
replace uniqueid=20124 if municipality =="SAN BLAS ATEMPA"
replace uniqueid=20130 if municipality =="SAN DIONISIO DEL MAR"
replace uniqueid=20134 if municipality =="SAN FELIPE JALAPA DE DIAZ"
replace uniqueid=20136 if municipality =="SAN FELIPE USILA"
replace uniqueid=20141 if municipality =="SAN FRANCISCO DEL MAR"
replace uniqueid=20143 if municipality =="SAN FRANCISCO IXHUATAN"
replace uniqueid=20150 if municipality =="SAN FRANCISCO TELIXTLAHUACA"
replace uniqueid=20157 if municipality =="SAN JACINTO AMILPAS"
replace uniqueid=20160 if municipality =="SAN JERONIMO SILACAYOAPILLA"
replace uniqueid=20166 if municipality =="SAN JOSE CHILTEPEC"
replace uniqueid=20168 if municipality =="SAN JOSE ESTANCIA GRANDE"
replace uniqueid=20169 if municipality =="SAN JOSE INDEPENDENCIA"
replace uniqueid=20171 if municipality =="SAN JOSE TENANGO"
replace uniqueid=20177 if municipality =="SAN JUAN BAUTISTA CUICATLAN"
replace uniqueid=20180 if municipality =="SAN JUAN BAUTISTA LO DE SOTO"
replace uniqueid=20181 if municipality =="SAN JUAN BAUTISTA SUCHITEPEC"
replace uniqueid=20182 if municipality =="SAN JUAN BTTA TLACOATZINTEPEC"
replace uniqueid=20184 if municipality =="SAN JUAN BAUTISTA TUXTEPEC"
replace uniqueid=20559 if municipality =="SAN JUAN BAUTISTA VALLE NACIONAL"
replace uniqueid=20185 if municipality =="SAN JUAN CACAHUATEPEC"
replace uniqueid=20187 if municipality =="SAN JUAN COATZOSPAM"
replace uniqueid=20188 if municipality =="SAN JUAN COLORADO"
replace uniqueid=20198 if municipality =="SAN JUAN GUICHICOVI"
replace uniqueid=20199 if municipality =="SAN JUAN IHUALTEPEC"
replace uniqueid=20225 if municipality =="SAN LORENZO"
replace uniqueid=20232 if municipality =="SAN LUCAS OJITLAN"
replace uniqueid=20237 if municipality =="SAN MARCOS ARTEAGA"
replace uniqueid=20245 if municipality =="SAN MARTIN ZACATEPEC"
replace uniqueid=20254 if municipality =="SAN MATEO RIO HONDO"
replace uniqueid=20259 if municipality =="SAN MIGUEL AHUEHUETITLAN"
replace uniqueid=20261 if municipality =="SAN MIGUEL AMATITLAN"
replace uniqueid=20278 if municipality =="SAN MIGUEL SOYALTEPEC"
replace uniqueid=20285 if municipality =="SAN MIGUEL TLACAMAMA"
replace uniqueid=20290 if municipality =="SAN NICOLAS HIDALGO"
replace uniqueid=20294 if municipality =="SAN PABLO HUITZO"
replace uniqueid=20295 if municipality =="SAN PABLO HUIXTEPEC"
replace uniqueid=20298 if municipality =="SAN PABLO VILLA DE MITLA"
replace uniqueid=20300 if municipality =="SAN PEDRO AMUZGOS"
replace uniqueid=20302 if municipality =="SAN PEDRO ATOYAC"
replace uniqueid=20305 if municipality =="SAN PEDRO COMITANCILLO"
replace uniqueid=20307 if municipality =="SAN PEDRO HUAMELULA"
replace uniqueid=20308 if municipality =="SAN PEDRO HUILOTEPEC"
replace uniqueid=20309 if municipality =="SAN PEDRO IXCATLAN"
replace uniqueid=20312 if municipality =="SAN PEDRO JICAYAN"
replace uniqueid=20318 if municipality =="SAN PEDRO MIXTEPEC"
replace uniqueid=20324 if municipality =="SAN PEDRO POCHUTLA"
replace uniqueid=20327 if municipality =="SAN PEDRO TAPANATEPEC"
replace uniqueid=20334 if municipality =="SAN PEDRO TUTUTEPEC"
replace uniqueid=20339 if municipality =="SAN PEDRO Y SAN PABLO TEPOSCOLULA"
replace uniqueid=20345 if municipality =="SAN SEBASTIAN IXCAPA"
replace uniqueid=20360 if municipality =="SANTA ANA ZEGACHE"
replace uniqueid=20364 if municipality =="SANTA CATARINA JUQUILA"
replace uniqueid=20375 if municipality =="SANTA CRUZ AMILPAS"
replace uniqueid=20377 if municipality =="SANTA CRUZ ITUNDUJIA"
replace uniqueid=20381 if municipality =="SANTA CRUZ TACACHE DE MINA"
replace uniqueid=20385 if municipality =="SANTA CRUZ XOXOCOTLAN"
replace uniqueid=20505 if municipality =="SANTA DOMINGO INGENIO"
replace uniqueid=20387 if municipality =="SANTA GERTRUDIS"
replace uniqueid=20390 if municipality =="SANTA LUCIA DEL CAMINO"
replace uniqueid=20402 if municipality =="SANTA MARIA CORTIJOS"
replace uniqueid=20413 if municipality =="SANTA MARIA HUATULCO"
replace uniqueid=20414 if municipality =="SANTA MARIA HUAZOLOTITLAN"
replace uniqueid=20415 if municipality =="SANTA MARIA IPALAPA"
replace uniqueid=20417 if municipality =="SANTA MARIA JACATEPEC"
replace uniqueid=20418 if municipality =="SANTA MARIA JALAPA DEL MARQUES"
replace uniqueid=20421 if municipality =="SANTA MARIA MIXTEQUILLA"
replace uniqueid=20427 if municipality =="SANTA MARIA PETAPA"
replace uniqueid=20431 if municipality =="SANTA MARIA TECOMAVACA"
replace uniqueid=20434 if municipality =="SANTA MARIA TEOPOXCO"
replace uniqueid=20436 if municipality =="SANTA MARIA TEXCATITLAN"
replace uniqueid=20439 if municipality =="SANTA MARIA TONAMECA"
replace uniqueid=20441 if municipality =="SANTA MARIA XADANI"
replace uniqueid=20447 if municipality =="SANTA MARIA ZACATEPEC"
replace uniqueid=20455 if municipality =="SANTIAGO AYUQUILILLA"
replace uniqueid=20456 if municipality =="SANTIAGO CACALOXTEPEC"
replace uniqueid=20459 if municipality =="SANTIAGO CHAZUMBA"
replace uniqueid=20462 if municipality =="SANTIAGO HUAJOLOTITLAN"
replace uniqueid=20467 if municipality =="SANTIAGO JAMILTEPEC"
replace uniqueid=20469 if municipality =="SANTIAGO JUXTLAHUACA"
replace uniqueid=20474 if municipality =="SANTIAGO LLANO GRANDE"
replace uniqueid=20472 if municipality =="SANTIAGO LAOLLAGA"
replace uniqueid=20476 if municipality =="SANTIAGO NILTEPEC"
replace uniqueid=20482 if municipality =="SANTIAGO PINOTEPA NACIONAL"
replace uniqueid=20483 if municipality =="SANTIAGO SUCHILQUITONGO"
replace uniqueid=20484 if municipality =="SANTIAGO TAMAZOLA"
replace uniqueid=20485 if municipality =="SANTIAGO TAPEXTLA"
replace uniqueid=20489 if municipality =="SANTIAGO TETEPEC"
replace uniqueid=20507 if municipality =="SANTO DOMINGO ARMENTA"
replace uniqueid=20508 if municipality =="SANTO DOMINGO CHIHUITAN"
replace uniqueid=20513 if municipality =="SANTO DOMINGO PETAPA"
replace uniqueid=20515 if municipality =="SANTO DOMINGO TEHUANTEPEC"
replace uniqueid=20520 if municipality =="SANTO DOMINGO TONALA"
replace uniqueid=20525 if municipality =="SANTO DOMINGO ZANATEPEC"
replace uniqueid=20537 if municipality =="SILACAYOAPAM"
replace uniqueid=20539 if municipality =="SOLEDAD ETLA"
replace uniqueid=20545 if municipality =="TEOTITLAN DE FLORES MAGON"
replace uniqueid=20549 if municipality =="TEZOATLAN DE SEGURA Y LUNA"
replace uniqueid=20551 if municipality =="TLACOLULA DE MATAMOROS"
replace uniqueid=20555 if municipality =="TRINIDAD ZAACHILA"
replace uniqueid=20557 if municipality =="UNION HIDALGO"
replace uniqueid=20558 if municipality =="VALERIO TRUJANO"
replace uniqueid=20338 if municipality =="VILLA DE ETLA"
replace uniqueid=20505 if municipality =="SANTO DOMINGO INGENIO"
replace uniqueid=20520 if strpos(municipality, "SANTO DOMINGO TONALA")>0
replace uniqueid=20540 if municipality =="TAMAZULAPAM DEL PROGRESO"
replace uniqueid=20565 if municipality =="VILLA DE ZAACHILA"
replace uniqueid=20277 if municipality =="VILLA SOLA DE VEGA"
replace uniqueid=20486 if municipality =="TEJUPAM DE LA UNION"
replace uniqueid=20567 if municipality =="ZAPOTITLAN LAGUNAS"
replace uniqueid=20568 if municipality =="ZIMATLAN DE ALVAREZ"

foreach var in PAN PRI PRD PT PVEM PARMEO PC total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PARMEO inv_mun_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PARMEO_r PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PARMEO PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1998
gen month ="Agosto"

save Oaxaca_1998.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Concejales 2001.xls", sheet("Votacion por casilla Municipal") cellrange(A2:U2820) firstrow clear

drop if Municipio=="Total del Municipio:"
drop if Cve==""

replace Municipio = Municipio[_n-1] if Cve == Cve[_n-1]

drop if strpos(PAN, "ANULADA")>0 | strpos(PAN, "DESTRUIDA")>0 | strpos(PAN, "INSTALO")>0 
drop if strpos(ListaNominal, "ANULADA")>0 | strpos(ListaNominal, "INSTALO")>0
foreach var in PAN PRD PT PVEM PSN CDPPN PAS {
replace `var' = subinstr(`var', "-", "", .)
}
destring ListaNominal PAN-PAS, replace
rename ListaNominal listanominal
rename CDPPN PC

rename Secc section 
destring section, replace
rename Municipio municipality
rename VTotalEmitida total
rename TVotosVlidos valid

drop Cve NoReg Nulos TdeAbst Vot Abst U

collapse (sum) listanominal - total , by(municipality section)

gen turnout =  total/listanominal

replace municipality = subinstr(municipality, "á", "a", .)
replace municipality = subinstr(municipality, "é", "e", .)
replace municipality = subinstr(municipality, "í", "i", .)
replace municipality = subinstr(municipality, "ó", "o", .)

replace municipality = upper(municipality)
gen   uniqueid= 0
replace uniqueid=20002 if municipality =="ACATLAN DE PEREZ FIGUEROA"
replace uniqueid=20004 if municipality =="ASUNCION CUYOTEPEJI"
replace uniqueid=20005 if municipality =="ASUNCION IXTALTEPEC"
replace uniqueid=20006 if municipality =="ASUNCION NOCHIXTLAN"
replace uniqueid=20007 if municipality =="ASUNCION OCOTLAN"
replace uniqueid=20009 if municipality =="AYOTZINTEPEC"
replace uniqueid=20025 if municipality =="CHAHUITES"
replace uniqueid=20026 if municipality =="CHALCATONGO DE HIDALGO"
replace uniqueid=20013 if municipality =="CIENEGA DE ZIMATLAN"
replace uniqueid=20014 if municipality =="CIUDAD IXTEPEC"
replace uniqueid=20021 if municipality =="COSOLAPA"
replace uniqueid=20023 if municipality =="CUILAPAM DE GUERRERO"
replace uniqueid=20028 if municipality =="EJUTLA DE CRESPO"
replace uniqueid=20010 if municipality =="BARRIO DE LA SOLEDAD, EL"
replace uniqueid=20030 if municipality =="EL ESPINAL"
replace uniqueid=20032 if municipality =="FRESNILLO DE TRUJANO"
replace uniqueid=20034 if municipality =="GUADALUPE DE RAMIREZ"
replace uniqueid=20397 if municipality =="H. CIUDAD DE TLAXIACO"
replace uniqueid=20039 if municipality =="HUAJUAPAN DE LEON"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20053 if municipality =="MAGDALENA TLACOTEPEC"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO"
replace uniqueid=20059 if municipality =="MIAHUATLAN DE PORFIRIO DIAZ"
replace uniqueid=20067 if municipality =="OAXACA DE JUAREZ"
replace uniqueid=20068 if municipality =="OCOTLAN DE MORELOS"
replace uniqueid=20070 if municipality =="PINOTEPA DE DON LUIS"
replace uniqueid=20073 if municipality =="PUTLA VILLA DE GUERRERO"
replace uniqueid=20075 if municipality =="REFORMA DE PINEDA"
replace uniqueid=20079 if municipality =="SALINA CRUZ"
replace uniqueid=20080 if municipality =="SAN AGUSTIN AMATENGO"
replace uniqueid=20081 if municipality =="SAN AGUSTIN ATENANGO"
replace uniqueid=20089 if municipality =="SAN ANDRES DINICUITI"
replace uniqueid=20090 if municipality =="SAN ANDRES HUAXPALTEPEC"
replace uniqueid=20102 if municipality =="SAN ANDRES ZAUTLA"
replace uniqueid=20103 if municipality =="SAN ANTONINO CASTILLO VELASCO"
replace uniqueid=20112 if municipality =="SAN BALTAZAR CHICHICAPAM"
replace uniqueid=20116 if municipality =="SAN BARTOLOME AYAUTLA"
replace uniqueid=20124 if municipality =="SAN BLAS ATEMPA"
replace uniqueid=20130 if municipality =="SAN DIONISIO DEL MAR"
replace uniqueid=20134 if municipality =="SAN FELIPE JALAPA DE DIAZ"
replace uniqueid=20136 if municipality =="SAN FELIPE USILA"
replace uniqueid=20141 if municipality =="SAN FRANCISCO DEL MAR"
replace uniqueid=20143 if municipality =="SAN FRANCISCO IXHUATAN"
replace uniqueid=20150 if municipality =="SAN FRANCISCO TELIXTLAHUACA"
replace uniqueid=20157 if municipality =="SAN JACINTO AMILPAS"
replace uniqueid=20160 if municipality =="SAN JERONIMO SILACAYOAPILLA"
replace uniqueid=20166 if municipality =="SAN JOSE CHILTEPEC"
replace uniqueid=20168 if municipality =="SAN JOSE ESTANCIA GRANDE"
replace uniqueid=20169 if municipality =="SAN JOSE INDEPENDENCIA"
replace uniqueid=20171 if municipality =="SAN JOSE TENANGO"
replace uniqueid=20177 if municipality =="SAN JUAN BAUTISTA CUICATLAN"
replace uniqueid=20180 if municipality =="SAN JUAN BAUTISTA LO DE SOTO"
replace uniqueid=20181 if municipality =="SAN JUAN BAUTISTA SUCHITEPEC"
replace uniqueid=20182 if municipality =="SAN JUAN BTTA TLACOATZINTEPEC"
replace uniqueid=20184 if municipality =="SAN JUAN BAUTISTA TUXTEPEC"
replace uniqueid=20559 if municipality =="SAN JUAN BAUTISTA VALLE NACIONAL"
replace uniqueid=20185 if municipality =="SAN JUAN CACAHUATEPEC"
replace uniqueid=20187 if municipality =="SAN JUAN COATZOSPAM"
replace uniqueid=20188 if municipality =="SAN JUAN COLORADO"
replace uniqueid=20198 if municipality =="SAN JUAN GUICHICOVI"
replace uniqueid=20199 if municipality =="SAN JUAN IHUALTEPEC"
replace uniqueid=20225 if municipality =="SAN LORENZO"
replace uniqueid=20232 if municipality =="SAN LUCAS OJITLAN"
replace uniqueid=20237 if municipality =="SAN MARCOS ARTEAGA"
replace uniqueid=20245 if municipality =="SAN MARTIN ZACATEPEC"
replace uniqueid=20254 if municipality =="SAN MATEO RIO HONDO"
replace uniqueid=20259 if municipality =="SAN MIGUEL AHUEHUETITLAN"
replace uniqueid=20261 if municipality =="SAN MIGUEL AMATITLAN"
replace uniqueid=20278 if municipality =="SAN MIGUEL SOYALTEPEC"
replace uniqueid=20285 if municipality =="SAN MIGUEL TLACAMAMA"
replace uniqueid=20290 if municipality =="SAN NICOLAS HIDALGO"
replace uniqueid=20294 if municipality =="SAN PABLO HUITZO"
replace uniqueid=20295 if municipality =="SAN PABLO HUIXTEPEC"
replace uniqueid=20298 if municipality =="SAN PABLO VILLA DE MITLA"
replace uniqueid=20300 if municipality =="SAN PEDRO AMUZGOS"
replace uniqueid=20302 if municipality =="SAN PEDRO ATOYAC"
replace uniqueid=20305 if municipality =="SAN PEDRO COMITANCILLO"
replace uniqueid=20307 if municipality =="SAN PEDRO HUAMELULA"
replace uniqueid=20308 if municipality =="SAN PEDRO HUILOTEPEC"
replace uniqueid=20309 if municipality =="SAN PEDRO IXCATLAN"
replace uniqueid=20312 if municipality =="SAN PEDRO JICAYAN"
replace uniqueid=20318 if municipality =="SAN PEDRO MIXTEPEC"
replace uniqueid=20324 if municipality =="SAN PEDRO POCHUTLA"
replace uniqueid=20327 if municipality =="SAN PEDRO TAPANATEPEC"
replace uniqueid=20334 if municipality =="SAN PEDRO TUTUTEPEC"
replace uniqueid=20339 if municipality =="SAN PEDRO Y SAN PABLO TEPOSCOLULA"
replace uniqueid=20345 if municipality =="SAN SEBASTIAN IXCAPA"
replace uniqueid=20360 if municipality =="SANTA ANA ZEGACHE"
replace uniqueid=20364 if municipality =="SANTA CATARINA JUQUILA"
replace uniqueid=20375 if municipality =="SANTA CRUZ AMILPAS"
replace uniqueid=20377 if municipality =="SANTA CRUZ ITUNDUJIA"
replace uniqueid=20381 if municipality =="SANTA CRUZ TACACHE DE MINA"
replace uniqueid=20385 if municipality =="SANTA CRUZ XOXOCOTLAN"
replace uniqueid=20505 if municipality =="SANTA DOMINGO INGENIO"
replace uniqueid=20387 if municipality =="SANTA GERTRUDIS"
replace uniqueid=20390 if municipality =="SANTA LUCIA DEL CAMINO"
replace uniqueid=20402 if municipality =="SANTA MARIA CORTIJOS"
replace uniqueid=20413 if municipality =="SANTA MARIA HUATULCO"
replace uniqueid=20414 if municipality =="SANTA MARIA HUAZOLOTITLAN"
replace uniqueid=20415 if municipality =="SANTA MARIA IPALAPA"
replace uniqueid=20417 if municipality =="SANTA MARIA JACATEPEC"
replace uniqueid=20418 if municipality =="SANTA MARIA JALAPA DEL MARQUES"
replace uniqueid=20421 if municipality =="SANTA MARIA MIXTEQUILLA"
replace uniqueid=20427 if municipality =="SANTA MARIA PETAPA"
replace uniqueid=20431 if municipality =="SANTA MARIA TECOMAVACA"
replace uniqueid=20434 if municipality =="SANTA MARIA TEOPOXCO"
replace uniqueid=20436 if municipality =="SANTA MARIA TEXCATITLAN"
replace uniqueid=20439 if municipality =="SANTA MARIA TONAMECA"
replace uniqueid=20441 if municipality =="SANTA MARIA XADANI"
replace uniqueid=20447 if municipality =="SANTA MARIA ZACATEPEC"
replace uniqueid=20455 if municipality =="SANTIAGO AYUQUILILLA"
replace uniqueid=20456 if municipality =="SANTIAGO CACALOXTEPEC"
replace uniqueid=20459 if municipality =="SANTIAGO CHAZUMBA"
replace uniqueid=20462 if municipality =="SANTIAGO HUAJOLOTITLAN"
replace uniqueid=20467 if municipality =="SANTIAGO JAMILTEPEC"
replace uniqueid=20469 if municipality =="SANTIAGO JUXTLAHUACA"
replace uniqueid=20474 if municipality =="SANTIAGO LLANO GRANDE"
replace uniqueid=20472 if municipality =="SANTIAGO LAOLLAGA"
replace uniqueid=20476 if municipality =="SANTIAGO NILTEPEC"
replace uniqueid=20482 if municipality =="SANTIAGO PINOTEPA NACIONAL"
replace uniqueid=20483 if municipality =="SANTIAGO SUCHILQUITONGO"
replace uniqueid=20484 if municipality =="SANTIAGO TAMAZOLA"
replace uniqueid=20485 if municipality =="SANTIAGO TAPEXTLA"
replace uniqueid=20489 if municipality =="SANTIAGO TETEPEC"
replace uniqueid=20507 if municipality =="SANTO DOMINGO ARMENTA"
replace uniqueid=20508 if municipality =="SANTO DOMINGO CHIHUITAN"
replace uniqueid=20513 if municipality =="SANTO DOMINGO PETAPA"
replace uniqueid=20515 if municipality =="SANTO DOMINGO TEHUANTEPEC"
replace uniqueid=20520 if municipality =="SANTO DOMINGO TONALA"
replace uniqueid=20525 if municipality =="SANTO DOMINGO ZANATEPEC"
replace uniqueid=20537 if municipality =="SILACAYOAPAM"
replace uniqueid=20539 if municipality =="SOLEDAD ETLA"
replace uniqueid=20545 if municipality =="TEOTITLAN DE FLORES MAGON"
replace uniqueid=20549 if municipality =="TEZOATLAN DE SEGURA Y LUNA"
replace uniqueid=20551 if municipality =="TLACOLULA DE MATAMOROS"
replace uniqueid=20555 if municipality =="TRINIDAD ZAACHILA"
replace uniqueid=20557 if municipality =="UNION HIDALGO"
replace uniqueid=20558 if municipality =="VALERIO TRUJANO"
replace uniqueid=20338 if municipality =="VILLA DE ETLA"
replace uniqueid=20505 if municipality =="SANTO DOMINGO INGENIO"
replace uniqueid=20520 if strpos(municipality, "SANTO DOMINGO TONALA")>0
replace uniqueid=20540 if municipality =="TAMAZULAPAM DEL PROGRESO"
replace uniqueid=20565 if municipality =="VILLA DE ZAACHILA"
replace uniqueid=20277 if municipality =="VILLA SOLA DE VEGA"
replace uniqueid=20486 if municipality =="TEJUPAM DE LA UNION"
replace uniqueid=20567 if municipality =="ZAPOTITLAN LAGUNAS"
replace uniqueid=20568 if municipality =="ZIMATLAN DE ALVAREZ"


foreach var in PAN PRI PRD PT PVEM PSN PC PAS total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PSN inv_mun_PC inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PSN_r PC_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PSN PC PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2001
gen month ="Octubre"

save Oaxaca_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal -  total , replace

collapse (sum)  listanominal -  pup total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
rename pup  PUP

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=20002 if municipality =="ACATLAN DE PEREZ FIGUEROA"
replace uniqueid=20004 if municipality =="ASUNCION CUYOTEPEJI"
replace uniqueid=20005 if municipality =="ASUNCION IXTALTEPEC"
replace uniqueid=20006 if municipality =="ASUNCION NOCHIXTLAN"
replace uniqueid=20009 if municipality =="AYOTZINTEPEC"
replace uniqueid=20025 if municipality =="CHAHUITES"
replace uniqueid=20026 if municipality =="CHALCATONGO DE HIDALGO"
replace uniqueid=20013 if municipality =="CIENEGA DE ZIMATLAN"
replace uniqueid=20014 if municipality =="CIUDAD IXTEPEC"
replace uniqueid=20021 if municipality =="COSOLAPA"
replace uniqueid=20023 if municipality =="CUILAPAM DE GUERRERO"
replace uniqueid=20028 if municipality =="EJUTLA DE CRESPO"
replace uniqueid=20010 if municipality =="EL BARRIO DE LA SOLEDAD"
replace uniqueid=20030 if municipality =="EL ESPINAL"
replace uniqueid=20032 if municipality =="FRESNILLO DE TRUJANO"
replace uniqueid=20034 if municipality =="GUADALUPE DE RAMIREZ"
replace uniqueid=20397 if municipality =="HEROICA CIUDAD DE TLAXIACO"
replace uniqueid=20039 if municipality =="HUAJUAPAN DE LEON"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO"
replace uniqueid=20059 if municipality =="MIAHUATLAN DE PORFIRIO DIAZ"
replace uniqueid=20067 if municipality =="OAXACA DE JUAREZ"
replace uniqueid=20068 if municipality =="OCOTLAN DE MORELOS"
replace uniqueid=20070 if municipality =="PINOTEPA DE DON LUIS"
replace uniqueid=20073 if municipality =="PUTLA VILLA DE GUERRERO"
replace uniqueid=20075 if municipality =="REFORMA DE PINEDA"
replace uniqueid=20079 if municipality =="SALINA CRUZ"
replace uniqueid=20080 if municipality =="SAN AGUSTIN AMATENGO"
replace uniqueid=20081 if municipality =="SAN AGUSTIN ATENANGO"
replace uniqueid=20089 if municipality =="SAN ANDRES DINICUITI"
replace uniqueid=20090 if municipality =="SAN ANDRES HUAXPALTEPEC"
replace uniqueid=20102 if municipality =="SAN ANDRES ZAUTLA"
replace uniqueid=20103 if municipality =="SAN ANTONINO CASTILLO VELASCO"
replace uniqueid=20112 if municipality =="SAN BALTAZAR CHICHICAPAM"
replace uniqueid=20116 if municipality =="SAN BARTOLOME AYAUTLA"
replace uniqueid=20124 if municipality =="SAN BLAS ATEMPA"
replace uniqueid=20130 if municipality =="SAN DIONISIO DEL MAR"
replace uniqueid=20134 if municipality =="SAN FELIPE JALAPA DE DIAZ"
replace uniqueid=20136 if municipality =="SAN FELIPE USILA"
replace uniqueid=20141 if municipality =="SAN FRANCISCO DEL MAR"
replace uniqueid=20143 if municipality =="SAN FRANCISCO IXHUATLAN"
replace uniqueid=20150 if municipality =="SAN FRANCISCO TELIXTLAHUACA"
replace uniqueid=20157 if municipality =="SAN JACINTO AMILPAS"
replace uniqueid=20160 if municipality =="SAN JERONIMO SILACAYOAPILLA"
replace uniqueid=20166 if municipality =="SAN JOSE CHILTEPEC"
replace uniqueid=20168 if municipality =="SAN JOSE ESTANCIA GRANDE"
replace uniqueid=20169 if municipality =="SAN JOSE INDEPENDENCIA"
replace uniqueid=20171 if municipality =="SAN JOSE TENANGO"
replace uniqueid=20177 if municipality =="SAN JUAN BAUTISTA CUICATLAN"
replace uniqueid=20180 if municipality =="SAN JUAN BAUTISTA LO DE SOTO"
replace uniqueid=20181 if municipality =="SAN JUAN BAUTISTA SUCHITEPEC"
replace uniqueid=20182 if municipality =="SAN JUAN BAUTISTA TLACOATZINTEPEC"
replace uniqueid=20184 if municipality =="SAN JUAN BAUTISTA TUXTEPEC"
replace uniqueid=20559 if municipality =="SAN JUAN BAUTISTA VALLE NACIONAL"
replace uniqueid=20185 if municipality =="SAN JUAN CACAHUATEPEC"
replace uniqueid=20187 if municipality =="SAN JUAN COATZOSPAM"
replace uniqueid=20188 if municipality =="SAN JUAN COLORADO"
replace uniqueid=20198 if municipality =="SAN JUAN GUICHICOVI"
replace uniqueid=20199 if municipality =="SAN JUAN IHUALTEPEC"
replace uniqueid=20225 if municipality =="SAN LORENZO"
replace uniqueid=20232 if municipality =="SAN LUCAS OJITLAN"
replace uniqueid=20237 if municipality =="SAN MARCOS ARTEAGA"
replace uniqueid=20245 if municipality =="SAN MARTIN ZACATEPEC"
replace uniqueid=20254 if municipality =="SAN MATEO RIO HONDO"
replace uniqueid=20259 if municipality =="SAN MIGUEL AHUEHUETITLAN"
replace uniqueid=20261 if municipality =="SAN MIGUEL AMATITLAN"
replace uniqueid=20278 if municipality =="SAN MIGUEL SOYALTEPEC"
replace uniqueid=20285 if municipality =="SAN MIGUEL TLACAMAMA"
replace uniqueid=20290 if municipality =="SAN NICOLAS HIDALGO"
replace uniqueid=20294 if municipality =="SAN PABLO HUITZO"
replace uniqueid=20295 if municipality =="SAN PABLO HUIXTEPEC"
replace uniqueid=20298 if municipality =="SAN PABLO VILLA DE MITLA"
replace uniqueid=20300 if municipality =="SAN PEDRO AMUZGOS"
replace uniqueid=20302 if municipality =="SAN PEDRO ATOYAC"
replace uniqueid=20305 if municipality =="SAN PEDRO COMITANCILLO"
replace uniqueid=20307 if municipality =="SAN PEDRO HUAMELULA"
replace uniqueid=20308 if municipality =="SAN PEDRO HUILOTEPEC"
replace uniqueid=20309 if municipality =="SAN PEDRO IXCATLAN"
replace uniqueid=20312 if municipality =="SAN PEDRO JICAYAN"
replace uniqueid=20318 if municipality =="SAN PEDRO MIXTEPEC"
replace uniqueid=20324 if municipality =="SAN PEDRO POCHUTLA"
replace uniqueid=20327 if municipality =="SAN PEDRO TAPANATEPEC"
replace uniqueid=20334 if municipality =="SAN PEDRO TUTUTEPEC"
replace uniqueid=20339 if municipality =="SAN PEDRO Y SAN PABLO TEPOSCOLULA"
replace uniqueid=20345 if municipality =="SAN SEBASTIAN IXCAPA"
replace uniqueid=20360 if municipality =="SANTA ANA ZEGACHE"
replace uniqueid=20364 if municipality =="SANTA CATARINA JUQUILA"
replace uniqueid=20375 if municipality =="SANTA CRUZ AMILPAS"
replace uniqueid=20377 if municipality =="SANTA CRUZ ITUNDUJIA"
replace uniqueid=20381 if municipality =="SANTA CRUZ TACACHE DE MINA"
replace uniqueid=20385 if municipality =="SANTA CRUZ XOXOCOTLAN"
replace uniqueid=20505 if municipality =="SANTA DOMINGO INGENIO"
replace uniqueid=20387 if municipality =="SANTA GERTRUDIS"
replace uniqueid=20390 if municipality =="SANTA LUCIA DEL CAMINO"
replace uniqueid=20402 if municipality =="SANTA MARIA CORTIJO"
replace uniqueid=20413 if municipality =="SANTA MARIA HUALTULCO"
replace uniqueid=20414 if municipality =="SANTA MARIA HUAZOLOTITLAN"
replace uniqueid=20415 if municipality =="SANTA MARIA IPALAPA"
replace uniqueid=20417 if municipality =="SANTA MARIA JACATEPEC"
replace uniqueid=20418 if municipality =="SANTA MARIA JALAPA DEL MARQUES"
replace uniqueid=20421 if municipality =="SANTA MARIA MIXTEQUILLA"
replace uniqueid=20427 if municipality =="SANTA MARIA PETAPA"
replace uniqueid=20431 if municipality =="SANTA MARIA TECOMAVACA"
replace uniqueid=20434 if municipality =="SANTA MARIA TEOPOXCO"
replace uniqueid=20436 if municipality =="SANTA MARIA TEXCATITLAN"
replace uniqueid=20439 if municipality =="SANTA MARIA TONAMECA"
replace uniqueid=20441 if municipality =="SANTA MARIA XADANI"
replace uniqueid=20447 if municipality =="SANTA MARIA ZACATEPEC"
replace uniqueid=20455 if municipality =="SANTIAGO AYUQUILILLA"
replace uniqueid=20456 if municipality =="SANTIAGO CACALOXTEPEC"
replace uniqueid=20459 if municipality =="SANTIAGO CHAZUMBA"
replace uniqueid=20462 if municipality =="SANTIAGO HUAJOLOTITLAN"
replace uniqueid=20467 if municipality =="SANTIAGO JAMILTEPEC"
replace uniqueid=20469 if municipality =="SANTIAGO JUXTLAHUACA"
replace uniqueid=20474 if municipality =="SANTIAGO LLANO GRANDE"
replace uniqueid=20476 if municipality =="SANTIAGO NILTEPEC"
replace uniqueid=20482 if municipality =="SANTIAGO PINOTEPA NACIONAL"
replace uniqueid=20483 if municipality =="SANTIAGO SUCHILQUITONGO"
replace uniqueid=20484 if municipality =="SANTIAGO TAMAZOLA"
replace uniqueid=20485 if municipality =="SANTIAGO TAPEXTLA"
replace uniqueid=20489 if municipality =="SANTIAGO TETEPEC"
replace uniqueid=20507 if municipality =="SANTO DOMINGO ARMENTA"
replace uniqueid=20508 if municipality =="SANTO DOMINGO CHIHUITAN"
replace uniqueid=20513 if municipality =="SANTO DOMINGO PETAPA"
replace uniqueid=20515 if municipality =="SANTO DOMINGO TEHUANTEPEC"
replace uniqueid=20520 if municipality =="SANTO DOMINGO TONALA"
replace uniqueid=20525 if municipality =="SANTO DOMINGO ZANATEPEC"
replace uniqueid=20537 if municipality =="SILACAYOAPAM"
replace uniqueid=20539 if municipality =="SOLEDAD ETLA"
replace uniqueid=20545 if municipality =="TEOTITLAN DE FLORES MAGON"
replace uniqueid=20549 if municipality =="TEZOATLAN DE SEGURA Y LUNA"
replace uniqueid=20551 if municipality =="TLACOLULA DE MATAMOROS"
replace uniqueid=20555 if municipality =="TRINIDAD ZAACHILA"
replace uniqueid=20557 if municipality =="UNION HIDALGO"
replace uniqueid=20558 if municipality =="VALERIO TRUJANO"
replace uniqueid=20338 if municipality =="VILLA DE ETLA"
replace uniqueid=20540 if municipality =="VILLA DE TAMAZULAPAM DEL PROGRESO"
replace uniqueid=20565 if municipality =="VILLA DE ZAACHILA"
replace uniqueid=20277 if municipality =="VILLA SOLA DE VEGA"
replace uniqueid=20486 if municipality =="VILLA TEJUPAM DE LA UNION"
replace uniqueid=20567 if municipality =="ZAPOTITLAN LAGUNAS"
replace uniqueid=20568 if municipality =="ZIMATLAN DE ALVAREZ"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PUP)

foreach var in PAN PRI PRD PT PVEM PC PUP total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PUP, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PUP_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PUP {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2004
gen month ="October"

save Oaxaca_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal -  total , replace

collapse (sum)  listanominal -  total , by (municipality section)

rename pan PAN
rename pri PRI
rename prd  PRD
rename pt  PT
rename pvem PVEM
rename pc PC
rename pup PUP
rename pna PANAL
rename pasdc PAS

gen turnout =  total/listanominal

drop noregistrados nulos

gen   uniqueid= 0
replace uniqueid=20002 if municipality =="ACATLAN DE PEREZ FIGUEROA"
replace uniqueid=20004 if municipality =="ASUNCION CUYOTEPEJI"
replace uniqueid=20005 if municipality =="ASUNCION IXTALTEPEC"
replace uniqueid=20006 if municipality =="ASUNCION NOCHIXTLAN"
replace uniqueid=20007 if municipality =="ASUNCION OCOTLAN"
replace uniqueid=20009 if municipality =="AYOTZINTEPEC"
replace uniqueid=20025 if municipality =="CHAHUITES"
replace uniqueid=20026 if municipality =="CHALCATONGO DE HIDALGO"
replace uniqueid=20013 if municipality =="CIENEGA DE ZIMATLAN"
replace uniqueid=20014 if municipality =="CIUDAD IXTEPEC"
replace uniqueid=20021 if municipality =="COSOLAPA"
replace uniqueid=20023 if municipality =="CUILAPAM DE GUERRERO"
replace uniqueid=20010 if municipality =="EL BARRIO DE LA SOLEDAD"
replace uniqueid=20030 if municipality =="EL ESPINAL"
replace uniqueid=20032 if municipality =="FRESNILLO DE TRUJANO"
replace uniqueid=20034 if municipality =="GUADALUPE DE RAMIREZ"
replace uniqueid=20028 if municipality =="H. CD. DE EJUTLA DE CRESPO"
replace uniqueid=20039 if municipality =="H. CD. DE HUAJUAPAN DE LEON"
replace uniqueid=20397 if municipality =="H. CIUDAD DE TLAXIACO"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20053 if municipality =="MAGDALENA TLACOTEPEC"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO AVENDAÑO"
replace uniqueid=20059 if municipality =="MIAHUATLAN DE PORFIRIO DIAZ"
replace uniqueid=20067 if municipality =="OAXACA DE JUAREZ"
replace uniqueid=20068 if municipality =="OCOTLAN DE MORELOS"
replace uniqueid=20070 if municipality =="PINOTEPA DE DON LUIS"
replace uniqueid=20073 if municipality =="PUTLA VILLA DE GUERRERO"
replace uniqueid=20075 if municipality =="REFORMA DE PINEDA"
replace uniqueid=20079 if municipality =="SALINA CRUZ"
replace uniqueid=20080 if municipality =="SAN AGUSTIN AMATENGO"
replace uniqueid=20081 if municipality =="SAN AGUSTIN ATENANGO"
replace uniqueid=20089 if municipality =="SAN ANDRES DINICUITI"
replace uniqueid=20090 if municipality =="SAN ANDRES HUAXPALTEPEC"
replace uniqueid=20102 if municipality =="SAN ANDRES ZAUTLA"
replace uniqueid=20103 if municipality =="SAN ANTONINO CASTILLO VELASCO"
replace uniqueid=20112 if municipality =="SAN BALTAZAR CHICHICAPAM"
replace uniqueid=20116 if municipality =="SAN BARTOLOME AYAUTLA"
replace uniqueid=20124 if municipality =="SAN BLAS ATEMPA"
replace uniqueid=20130 if municipality =="SAN DIONISIO DEL MAR"
replace uniqueid=20134 if municipality =="SAN FELIPE JALAPA DE DIAZ"
replace uniqueid=20136 if municipality =="SAN FELIPE USILA"
replace uniqueid=20141 if municipality =="SAN FRANCISCO DEL MAR"
replace uniqueid=20143 if municipality =="SAN FRANCISCO IXHUATAN"
replace uniqueid=20150 if municipality =="SAN FRANCISCO TELIXTLAHUACA"
replace uniqueid=20157 if municipality =="SAN JACINTO AMILPAS"
replace uniqueid=20160 if municipality =="SAN JERONIMO SILACAYOAPILLA"
replace uniqueid=20166 if municipality =="SAN JOSE CHILTEPEC"
replace uniqueid=20168 if municipality =="SAN JOSE ESTANCIA GRANDE"
replace uniqueid=20169 if municipality =="SAN JOSE INDEPENDENCIA"
replace uniqueid=20171 if municipality =="SAN JOSE TENANGO"
replace uniqueid=20177 if municipality =="SAN JUAN BAUTISTA CUICATLAN"
replace uniqueid=20180 if municipality =="SAN JUAN BAUTISTA LO DE SOTO"
replace uniqueid=20181 if municipality =="SAN JUAN BAUTISTA SUCHITEPEC"
replace uniqueid=20182 if municipality =="SAN JUAN BAUTISTA TLACOATZINTEPEC"
replace uniqueid=20184 if municipality =="SAN JUAN BAUTISTA TUXTEPEC"
replace uniqueid=20559 if municipality =="SAN JUAN BAUTISTA VALLE NACIONAL"
replace uniqueid=20185 if municipality =="SAN JUAN CACAHUATEPEC"
replace uniqueid=20187 if municipality =="SAN JUAN COATZOSPAM"
replace uniqueid=20188 if municipality =="SAN JUAN COLORADO"
replace uniqueid=20198 if municipality =="SAN JUAN GUICHICOVI"
replace uniqueid=20199 if municipality =="SAN JUAN IHUALTEPEC"
replace uniqueid=20225 if municipality =="SAN LORENZO"
replace uniqueid=20232 if municipality =="SAN LUCAS OJITLAN"
replace uniqueid=20237 if municipality =="SAN MARCOS ARTEAGA"
replace uniqueid=20245 if municipality =="SAN MARTIN ZACATEPEC"
replace uniqueid=20254 if municipality =="SAN MATEO RIO HONDO"
replace uniqueid=20259 if municipality =="SAN MIGUEL AHUEHUETITLAN"
replace uniqueid=20261 if municipality =="SAN MIGUEL AMATITLAN"
replace uniqueid=20278 if municipality =="SAN MIGUEL SOYALTEPEC"
replace uniqueid=20285 if municipality =="SAN MIGUEL TLACAMAMA"
replace uniqueid=20290 if municipality =="SAN NICOLAS HIDALGO"
replace uniqueid=20294 if municipality =="SAN PABLO HUITZO"
replace uniqueid=20295 if municipality =="SAN PABLO HUIXTEPEC"
replace uniqueid=20298 if municipality =="SAN PABLO VILLA DE MITLA"
replace uniqueid=20300 if municipality =="SAN PEDRO AMUZGOS"
replace uniqueid=20302 if municipality =="SAN PEDRO ATOYAC"
replace uniqueid=20305 if municipality =="SAN PEDRO COMITANCILLO"
replace uniqueid=20307 if municipality =="SAN PEDRO HUAMELULA"
replace uniqueid=20308 if municipality =="SAN PEDRO HUILOTEPEC"
replace uniqueid=20309 if municipality =="SAN PEDRO IXCATLAN"
replace uniqueid=20312 if municipality =="SAN PEDRO JICAYAN"
replace uniqueid=20318 if municipality =="SAN PEDRO MIXTEPEC"
replace uniqueid=20324 if municipality =="SAN PEDRO POCHUTLA"
replace uniqueid=20327 if municipality =="SAN PEDRO TAPANATEPEC"
replace uniqueid=20339 if municipality =="SAN PEDRO Y SAN PABLO TEPOSCOLULA"
replace uniqueid=20345 if municipality =="SAN SEBASTIAN IXCAPA"
replace uniqueid=20360 if municipality =="SANTA ANA ZEGACHE"
replace uniqueid=20364 if municipality =="SANTA CATARINA JUQUILA"
replace uniqueid=20375 if municipality =="SANTA CRUZ AMILPAS"
replace uniqueid=20377 if municipality =="SANTA CRUZ ITUNDUJIA"
replace uniqueid=20381 if municipality =="SANTA CRUZ TACACHE DE MINA"
replace uniqueid=20385 if municipality =="SANTA CRUZ XOXOCOTLAN"
replace uniqueid=20387 if municipality =="SANTA GERTRUDIS"
replace uniqueid=20390 if municipality =="SANTA LUCIA DEL CAMINO"
replace uniqueid=20402 if municipality =="SANTA MARIA CORTIJO"
replace uniqueid=20413 if municipality =="SANTA MARIA HUATULCO"
replace uniqueid=20414 if municipality =="SANTA MARIA HUAZOLOTITLAN"
replace uniqueid=20415 if municipality =="SANTA MARIA IPALAPA"
replace uniqueid=20417 if municipality =="SANTA MARIA JACATEPEC"
replace uniqueid=20418 if municipality =="SANTA MARIA JALAPA DEL MARQUES"
replace uniqueid=20421 if municipality =="SANTA MARIA MIXTEQUILLA"
replace uniqueid=20427 if municipality =="SANTA MARIA PETAPA"
replace uniqueid=20431 if municipality =="SANTA MARIA TECOMAVACA"
replace uniqueid=20434 if municipality =="SANTA MARIA TEOPOXCO"
replace uniqueid=20436 if municipality =="SANTA MARIA TEXCATITLAN"
replace uniqueid=20439 if municipality =="SANTA MARIA TONAMECA"
replace uniqueid=20441 if municipality =="SANTA MARIA XADANI"
replace uniqueid=20447 if municipality =="SANTA MARIA ZACATEPEC"
replace uniqueid=20455 if municipality =="SANTIAGO AYUQUILILLA"
replace uniqueid=20456 if municipality =="SANTIAGO CACALOXTEPEC"
replace uniqueid=20459 if municipality =="SANTIAGO CHAZUMBA"
replace uniqueid=20462 if municipality =="SANTIAGO HUAJOLOTITLAN"
replace uniqueid=20467 if municipality =="SANTIAGO JAMILTEPEC"
replace uniqueid=20469 if municipality =="SANTIAGO JUXTLAHUACA"
replace uniqueid=20472 if municipality =="SANTIAGO LAOLLAGA"
replace uniqueid=20474 if municipality =="SANTIAGO LLANO GRANDE"
replace uniqueid=20476 if municipality =="SANTIAGO NILTEPEC"
replace uniqueid=20482 if municipality =="SANTIAGO PINOTEPA NACIONAL"
replace uniqueid=20483 if municipality =="SANTIAGO SUCHILQUITONGO"
replace uniqueid=20484 if municipality =="SANTIAGO TAMAZOLA"
replace uniqueid=20485 if municipality =="SANTIAGO TAPEXTLA"
replace uniqueid=20489 if municipality =="SANTIAGO TETEPEC"
replace uniqueid=20507 if municipality =="SANTO DOMINGO ARMENTA"
replace uniqueid=20508 if municipality =="SANTO DOMINGO CHIHUITAN"
replace uniqueid=20505 if municipality =="SANTO DOMINGO INGENIO"
replace uniqueid=20513 if municipality =="SANTO DOMINGO PETAPA"
replace uniqueid=20515 if municipality =="SANTO DOMINGO TEHUANTEPEC"
replace uniqueid=20520 if municipality =="SANTO DOMINGO TONALA"
replace uniqueid=20525 if municipality =="SANTO DOMINGO ZANATEPEC"
replace uniqueid=20537 if municipality =="SILACAYOAPAM"
replace uniqueid=20539 if municipality =="SOLEDAD ETLA"
replace uniqueid=20545 if municipality =="TEOTITLAN DE FLORES MAGON"
replace uniqueid=20549 if municipality =="TEZOATLAN DE SEGURA Y LUNA"
replace uniqueid=20551 if municipality =="TLACOLULA DE MATAMOROS"
replace uniqueid=20555 if municipality =="TRINIDAD ZAACHILA"
replace uniqueid=20557 if municipality =="UNION HIDALGO"
replace uniqueid=20558 if municipality =="VALERIO TRUJANO"
replace uniqueid=20338 if municipality =="VILLA DE ETLA"
replace uniqueid=20540 if municipality =="VILLA DE TAMAZULAPAM DEL PROGRESO"
replace uniqueid=20334 if municipality =="VILLA DE TUTUTEPEC DE MELCHOR OCAMPO"
replace uniqueid=20565 if municipality =="VILLA DE ZAACHILA"
replace uniqueid=20277 if municipality =="VILLA SOLA DE VEGA"
replace uniqueid=20486 if municipality =="VILLA TEJUPAM DE LA UNION"
replace uniqueid=20567 if municipality =="ZAPOTITLAN LAGUNAS"
replace uniqueid=20570 if municipality =="ZIMATLAN DE ALVAREZ"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PUP PANAL PAS)

foreach var in PAN PRI PRD PT PVEM PC PUP PANAL PAS total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PUP inv_mun_PANAL inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PUP_r PANAL_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PUP PANAL PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2007
gen month ="October"

sort section

save Oaxaca_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

use Ayu_Seccion_2010.dta, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal -  total , replace

collapse (sum)  listanominal - pna total , by (municipality section coalicion_1 coalicion_2)


**************************************************************************

gen pan_prd_pt_pc = 0
replace pan_prd_pt_pc = pan+ prd+pt+pc if coalicion_1=="PAN-PRD-PT-PC"
replace pan = 0 if coalicion_1=="PAN-PRD-PT-PC" 
replace prd = 0 if coalicion_1=="PAN-PRD-PT-PC"
replace pt = 0 if coalicion_1=="PAN-PRD-PT-PC"
replace pc = 0 if coalicion_1=="PAN-PRD-PT-PC"
sum pan prd pt pc
drop pan prd pt pc

gen pri_pvem = 0
replace pri_pvem = pri+pvem if coalicion_2=="PRI-PVEM"
replace pri  = 0 if coalicion_2=="PRI-PVEM"
replace pvem = 0 if coalicion_2=="PRI-PVEM"
sum pri pvem
drop pri pvem

drop coalicion_1 coalicion_2
**************************************************************************

rename pan_prd_pt_pc  PAN_PRD_PT_PC
rename pri_pvem PRI_PVEM
rename pup  PUP
rename pna  PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=20002 if municipality =="ACATLAN DE PEREZ FIGUEROA"
replace uniqueid=20004 if municipality =="ASUNCION CUYOTEPEJI"
replace uniqueid=20005 if municipality =="ASUNCION IXTALTEPEC"
replace uniqueid=20006 if municipality =="ASUNCION NOCHIXTLAN"
replace uniqueid=20007 if municipality =="ASUNCION OCOTLAN"
replace uniqueid=20009 if municipality =="AYOTZINTEPEC"
replace uniqueid=20025 if municipality =="CHAHUITES"
replace uniqueid=20026 if municipality =="CHALCATONGO DE HIDALGO"
replace uniqueid=20013 if municipality =="CIENEGA DE ZIMATLAN"
replace uniqueid=20014 if municipality =="CIUDAD IXTEPEC"
replace uniqueid=20021 if municipality =="COSOLAPA"
replace uniqueid=20023 if municipality =="CUILAPAM DE GUERRERO"
replace uniqueid=20010 if municipality =="EL BARRIO DE LA SOLEDAD"
replace uniqueid=20030 if municipality =="EL ESPINAL"
replace uniqueid=20032 if municipality =="FRESNILLO DE TRUJANO"
replace uniqueid=20034 if municipality =="GUADALUPE DE RAMIREZ"
replace uniqueid=20028 if municipality =="H. CIUDAD EJUTLA DE CRESPO"
replace uniqueid=20039 if municipality =="H. CIUDAD DE HUAJUAPAN DE LEON"
replace uniqueid=20397 if municipality =="HEROICA CIUDAD DE TLAXIACO"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="H. CD. DE JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20053 if municipality =="MAGDALENA TLACOTEPEC"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO AVENDAÑO"
replace uniqueid=20059 if municipality =="MIAHUATLAN DE PORFIRIO DIAZ"
replace uniqueid=20067 if municipality =="OAXACA DE JUAREZ"
replace uniqueid=20068 if municipality =="OCOTLAN DE MORELOS"
replace uniqueid=20070 if municipality =="PINOTEPA DE DON LUIS"
replace uniqueid=20073 if municipality =="PUTLA VILLA DE GUERRERO"
replace uniqueid=20075 if municipality =="REFORMA DE PINEDA"
replace uniqueid=20079 if municipality =="SALINA CRUZ"
replace uniqueid=20080 if municipality =="SAN AGUSTIN AMATENGO"
replace uniqueid=20081 if municipality =="SAN AGUNTIN ATENANGO"
replace uniqueid=20089 if municipality =="SAN ANDRES DINICUITI"
replace uniqueid=20090 if municipality =="SAN ANDRES HUAXPALTEPEC"
replace uniqueid=20102 if municipality =="SAN ANDRES ZAUTLA"
replace uniqueid=20103 if municipality =="SAN ANTONINO CASTILLO VELASCO"
replace uniqueid=20112 if municipality =="SAN BALTAZAR CHICHICAPAM"
replace uniqueid=20116 if municipality =="SAN BARTOLOME AYAUTLA"
replace uniqueid=20124 if municipality =="SAN BLAS ATEMPA"
replace uniqueid=20130 if municipality =="SAN DIONISIO DEL MAR"
replace uniqueid=20134 if municipality =="SAN FELIPE JALAPA DE DIAZ"
replace uniqueid=20136 if municipality =="SAN FELIPE USILA"
replace uniqueid=20141 if municipality =="SAN FRANCISCO DEL MAR"
replace uniqueid=20143 if municipality =="SAN FRANCISCO IXHUATAN"
replace uniqueid=20150 if municipality =="SAN FRANCISCO TELIXTLAHUACA"
replace uniqueid=20157 if municipality =="SAN JACINTO AMILPAS"
replace uniqueid=20160 if municipality =="SAN JERONIMO SILACAYOAPILLA"
replace uniqueid=20166 if municipality =="SAN JOSE CHILTEPEC"
replace uniqueid=20168 if municipality =="SAN JOSE ESTANCIA GRANDE"
replace uniqueid=20169 if municipality =="SAN JOSE INDEPENDENCIA"
replace uniqueid=20171 if municipality =="SAN JOSE TENANGO"
replace uniqueid=20177 if municipality =="SAN JUAN BAUTISTA CUICATLAN"
replace uniqueid=20180 if municipality =="SAN JUAN BAUTISTA LO DE SOTO"
replace uniqueid=20181 if municipality =="SAN JUAN BAUTISTA SUCHITEPEC"
replace uniqueid=20182 if municipality =="SAN JUAN BAUTISTA TLACOATZINTEPEC"
replace uniqueid=20184 if municipality =="SAN JUAN BAUTISTA TUXTEPEC"
replace uniqueid=20559 if municipality =="SAN JUAN BAUTISTA VALLE NACIONAL"
replace uniqueid=20185 if municipality =="SAN JUAN CACAHUATEPEC"
replace uniqueid=20187 if municipality =="SAN JUAN COATZOSPAM"
replace uniqueid=20188 if municipality =="SAN JUAN COLORADO"
replace uniqueid=20198 if municipality =="SAN JUAN GUICHICOVI"
replace uniqueid=20199 if municipality =="SAN JUAN IHUALTEPEC"
replace uniqueid=20225 if municipality =="SAN LORENZO"
replace uniqueid=20232 if municipality =="SAN LUCAS OJITLAN"
replace uniqueid=20237 if municipality =="SAN MARCOS ARTEAGA"
replace uniqueid=20245 if municipality =="SAN MARTIN ZACATEPEC"
replace uniqueid=20254 if municipality =="SAN MATEO RIO HONDO"
replace uniqueid=20259 if municipality =="SAN MIGUEL AHUEHUETITLAN"
replace uniqueid=20261 if municipality =="SAN MIGUEL AMATITLAN"
replace uniqueid=20278 if municipality =="SAN MIGUEL SOYALTEPEC"
replace uniqueid=20285 if municipality =="SAN MIGUEL TLACAMAMA"
replace uniqueid=20290 if municipality =="SAN NICOLAS HIDALGO"
replace uniqueid=20294 if municipality =="SAN PABLO HUITZO"
replace uniqueid=20295 if municipality =="SAN PABLO HUIXTEPEC"
replace uniqueid=20298 if municipality =="SAN PABLO VILLA DE MITLA"
replace uniqueid=20300 if municipality =="SAN PEDRO AMUZGOS"
replace uniqueid=20302 if municipality =="SAN PEDRO ATOYAC"
replace uniqueid=20305 if municipality =="SAN PEDRO COMITANCILLO"
replace uniqueid=20307 if municipality =="SAN PEDRO HUAMELULA"
replace uniqueid=20308 if municipality =="SAN PEDRO HUILOTEPEC"
replace uniqueid=20309 if municipality =="SAN PEDRO IXCATLAN"
replace uniqueid=20312 if municipality =="SAN PEDRO JICAYAN"
replace uniqueid=20318 if municipality =="SAN PEDRO MIXTEPEC"
replace uniqueid=20324 if municipality =="SAN PEDRO POCHUTLA"
replace uniqueid=20327 if municipality =="SAN PEDRO TAPANATEPEC"
replace uniqueid=20339 if municipality =="SAN PEDRO Y SAN PABLO TEPOSCOLULA"
replace uniqueid=20345 if municipality =="SAN SEBASTIAN IXCAPA"
replace uniqueid=20360 if municipality =="SANTA ANA ZEGACHE"
replace uniqueid=20364 if municipality =="SANTA CATARINA JUQUILA"
replace uniqueid=20375 if municipality =="SANTA CRUZ AMILPAS"
replace uniqueid=20377 if municipality =="SANTA CRUZ ITUNDUJIA"
replace uniqueid=20381 if municipality =="SANTA CRUZ TACACHE DE MINA"
replace uniqueid=20385 if municipality =="SANTA CRUZ XOXOCOTLAN"
replace uniqueid=20387 if municipality =="SANTA GERTRUDIS"
replace uniqueid=20390 if municipality =="SANTA LUCIA DEL CAMINO"
replace uniqueid=20402 if municipality =="SANTA MARIA CORTIJO"
replace uniqueid=20413 if municipality =="SANTA MARIA HUATULCO"
replace uniqueid=20414 if municipality =="SANTA MARIA HUAZOLOTITLAN"
replace uniqueid=20415 if municipality =="SANTA MARIA IPALAPA"
replace uniqueid=20417 if municipality =="SANTA MARIA JACATEPEC"
replace uniqueid=20418 if municipality =="SANTA MARIA JALAPA DEL MARQUES"
replace uniqueid=20421 if municipality =="SANTA MARIA MIXTEQUILLA"
replace uniqueid=20427 if municipality =="SANTA MARIA PETAPA"
replace uniqueid=20431 if municipality =="SANTA MARIA TECOMAVACA"
replace uniqueid=20434 if municipality =="SANTA MARIA TEOPOXCO"
replace uniqueid=20436 if municipality =="SANTA MARIA TEXCATITLAN"
replace uniqueid=20439 if municipality =="SANTA MARIA TONAMECA"
replace uniqueid=20441 if municipality =="SANTA MARIA XADANI"
replace uniqueid=20447 if municipality =="SANTA MARIA ZACATEPEC"
replace uniqueid=20455 if municipality =="SANTIAGO AYUQUILILLA"
replace uniqueid=20456 if municipality =="SANTIAGO CACALOXTEPEC"
replace uniqueid=20459 if municipality =="SANTIAGO CHAZUMBA"
replace uniqueid=20462 if municipality =="SANTIAGO HUAJOLOTITLAN"
replace uniqueid=20467 if municipality =="SANTIAGO JAMILTEPEC"
replace uniqueid=20469 if municipality =="SANTIAGO JUXTLAHUACA"
replace uniqueid=20472 if municipality =="SANTIAGO LAOLLAGA"
replace uniqueid=20474 if municipality =="SANTIAGO LLANO GRANDE"
replace uniqueid=20476 if municipality =="SANTIAGO NILTEPEC"
replace uniqueid=20482 if municipality =="SANTIAGO PINOTEPA NACIONAL"
replace uniqueid=20483 if municipality =="SANTIAGO SUCHILQUITONGO"
replace uniqueid=20484 if municipality =="SANTIAGO TAMAZOLA"
replace uniqueid=20485 if municipality =="SANTIAGO TAPEXTLA"
replace uniqueid=20489 if municipality =="SANTIAGO TETEPEC"
replace uniqueid=20507 if municipality =="SANTO DOMINGO ARMENTA"
replace uniqueid=20508 if municipality =="SANTO DOMINGO CHIHUITAN"
replace uniqueid=20505 if municipality =="SANTO DOMINGO INGENIO"
replace uniqueid=20513 if municipality =="SANTO DOMINGO PETAPA"
replace uniqueid=20515 if municipality =="SANTO DOMINGO TEHUANTEPEC"
replace uniqueid=20520 if municipality =="SANTO DOMINGO TONALA"
replace uniqueid=20525 if municipality =="SANTO DOMINGO ZANATEPEC"
replace uniqueid=20537 if municipality =="SILACAYOAPAM"
replace uniqueid=20539 if municipality =="SOLEDAD ETLA"
replace uniqueid=20545 if municipality =="TEOTITLAN DE FLORES MAGON"
replace uniqueid=20549 if municipality =="TEZOATLAN DE SEGURA Y LUNA"
replace uniqueid=20551 if municipality =="TLACOLULA DE MATAMOROS"
replace uniqueid=20555 if municipality =="TRINIDAD ZAACHILA"
replace uniqueid=20557 if municipality =="UNION HIDALGO"
replace uniqueid=20558 if municipality =="VALERIO TRUJANO"
replace uniqueid=20338 if municipality =="VILLA DE ETLA"
replace uniqueid=20540 if municipality =="VILLA DE TAMAZULAPAM DEL PROGRESO"
replace uniqueid=20334 if municipality =="VILLA DE TUTUTEPEC DE MELCHOR OCAMPO"
replace uniqueid=20565 if municipality =="VILLA DE ZAACHILA"
replace uniqueid=20277 if municipality =="VILLA SOLA DE VEGA"
replace uniqueid=20486 if municipality =="VILLA TEJUPAM DE LA UNION"
replace uniqueid=20567 if municipality =="ZAPOTITLAN LAGUNAS"
replace uniqueid=20570 if municipality =="ZIMATLAN DE ALVAREZ"

egen valid = rowtotal(PAN_PRD_PT_PC PRI_PVEM PUP PANAL)

foreach var in PAN_PRD_PT_PC PRI_PVEM PUP PANAL total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PT_PC inv_mun_PRI_PVEM inv_mun_PUP inv_mun_PANAL, gen(PAN_PRD_PT_PC_r PRI_PVEM_r PUP_r PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD_PT_PC PRI_PVEM PUP PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2010
gen month ="July"

sort section

save Oaxaca_2010.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Oaxaca_1998.dta, clear
append using Oaxaca_2001.dta
append using Oaxaca_2004.dta
append using Oaxaca_2007.dta
append using Oaxaca_2010.dta

erase Oaxaca_1998.dta
erase Oaxaca_2001.dta
erase Oaxaca_2004.dta
erase Oaxaca_2007.dta
erase Oaxaca_2010.dta

tab winner, missing
br if winner==""
br if municipality=="SANTA CRUZ ITUNDUJIA" & year==2007
br if municipality=="SAN JOSE ESTANCIA GRANDE" & year==2010

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Oaxaca_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Oaxaca_ALL.dta, replace

