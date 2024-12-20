////////////////////////////////////////////
////////OAXACA PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Oaxaca"

//////////////////////////////////
////////////2013//////////////////
//////////////////////////////////

import excel using "Votacion Concejales 2013 I_XIV.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Votacion Concejales 2013 I_XIV.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if B=="" | B=="DTTO"
	foreach x of varlist I-Y {
		replace `x' = "0" if `x'==""
		replace `x' = "" if `x'=="N.P." | `x'=="P.N" | `x'=="N.P"
	}
	save "`sheetname'.dta", replace
}

import excel using "Votacion Concejales 2013 XV_XXV.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Votacion Concejales 2013 XV_XXV.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if B=="" | B=="DTTO"
	foreach x of varlist I-Y {
		replace `x' = "0" if `x'==""
		replace `x' = "" if `x'=="N.P." | `x'=="P.N" | `x'=="N.P"
	}
	save "`sheetname'.dta", replace
}

clear
append using "Dtto I.dta"
append using "Dtto II.dta"
append using "Dtto IV.dta"
append using "Dtto V.dta"
append using "Dtto VI.dta"
append using "Dtto VII.dta"
append using "Dtto VIII.dta"
append using "IX.dta"
append using "Dtto X.dta"
append using "Dtto XI.dta"
append using "Dtto XII.dta"
append using "Dtto XIII.dta"
append using "Dtto XIV.dta"
append using "Dtto XV.dta"
append using "Dtto XVI.dta"
append using "Dtto XVII.dta"
append using "Dtto XVIII.dta"
append using "Dtto XIX.dta"
append using "Dtto XXI.dta"
append using "Dtto XXII.dta"
append using "Dtto XXIII.dta"
append using "Dtto XXIV.dta"
append using "Dtto XXV.dta"

erase "Dtto I.dta"
erase "Dtto II.dta"
erase "Dtto IV.dta"
erase "Dtto V.dta"
erase "Dtto VI.dta"
erase "Dtto VII.dta"
erase "Dtto VIII.dta"
erase "IX.dta"
erase "Dtto X.dta"
erase "Dtto XI.dta"
erase "Dtto XII.dta"
erase "Dtto XIII.dta"
erase "Dtto XIV.dta"
erase "Dtto XV.dta"
erase "Dtto XVI.dta"
erase "Dtto XVII.dta"
erase "Dtto XVIII.dta"
erase "Dtto XIX.dta"
erase "Dtto XXI.dta"
erase "Dtto XXII.dta"
erase "Dtto XXIII.dta"
erase "Dtto XXIV.dta"
erase "Dtto XXV.dta"

rename D municipality
rename F section
replace section = subinstr(section, "*", "", .)
drop if section=="Sección" | section=="" | H=="Casilla anulada *" | strpos(I, "asilla")>0

destring _all, replace

rename H listanominal
rename N PMC
rename O PUP
rename P PANAL
rename Q PSD
g PAN_PRD_PT = I + K + M + R + S + T + U
g PRI_PVEM = J + L + V
drop I-M R-X
rename Y total

replace municipality = upper(municipality)
replace municipality = subinstr(municipality, "á", "A", .)
replace municipality = subinstr(municipality, "é", "E", .)
replace municipality = subinstr(municipality, "í", "I", .)
replace municipality = subinstr(municipality, "ó", "O", .)
replace municipality = subinstr(municipality, "ú", "U", .)
replace municipality = subinstr(municipality, "ñ", "N", .)
g uniqueid = .
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
replace uniqueid=20028 if municipality =="HEROICA CIUDAD DE EJUTLA DE CRESPO"
replace uniqueid=20039 if municipality =="HEROICA CIUDAD DE HUAJUAPAN DE LEON"
replace uniqueid=20397 if municipality =="HEROICA CIUDAD DE TLAXIACO"
replace uniqueid=20040 if municipality =="HUAUTEPEC"
replace uniqueid=20041 if municipality =="HUAUTLA DE JIMENEZ"
replace uniqueid=20043 if municipality =="HEROICA CIUDAD DE JUCHITAN DE ZARAGOZA"
replace uniqueid=20044 if municipality =="LOMA BONITA"
replace uniqueid=20049 if municipality =="MAGDALENA OCOTLAN"
replace uniqueid=20052 if municipality =="MAGDALENA TEQUISISTLAN"
replace uniqueid=20053 if municipality =="MAGDALENA TLACOTEPEC"
replace uniqueid=20055 if municipality =="MARISCALA DE JUAREZ"
replace uniqueid=20056 if municipality =="MARTIRES DE TACUBAYA"
replace uniqueid=20057 if municipality =="MATIAS ROMERO AVENDANO"
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
replace uniqueid=20459 if municipality =="VILLA DE SANTIAGO CHAZUMBA"
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
replace uniqueid=20549 if strpos(municipality, "TEZOATLAN DE SEGURA Y LUNA")>0
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
replace uniqueid=20570 if municipality =="ZIMATLAN DE ÁLVAREZ"
replace uniqueid=20088 if municipality =="SAN ANDRES CABECERA NUEVA"

collapse (sum) PAN_PRD_PT PRI_PVEM PMC-PSD listanominal total, by(municipality uniqueid section)

egen valid=rowtotal(PAN_PRD_PT PRI_PVEM PMC PUP PANAL PSD)

foreach var in PAN_PRD_PT PRI_PVEM PMC PUP PANAL PSD total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN_PRD_PT i_PRI_PVEM i_PMC i_PUP i_PANAL i_PSD, gen(PAN_PRD_PT_r PRI_PVEM_r PMC_r PUP_r PANAL_r PSD_r)
drop i_*

g winner = ""
g second = ""
g third = ""

foreach var in PAN_PRD_PT PRI_PVEM PMC PUP PANAL PSD {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

g year=2013
g month="July"
g STATE="OAXACA"

order STATE municipality section uniqueid *

save Oaxaca_Section_2013.dta, replace

//////////////////////////////////
////////////2014//////////////////
//////////////////////////////////

import excel "Extraordina Tlacamama 2014.xlsx", sheet("Extraordinaria") cellrange(A2:R8) firstrow clear

rename Sección section
drop if section==.
destring _all, replace

g PRI_PVEM = Coalición + Votacióndelospartidospolític + J
rename K PUP

rename ListaNominal listanominal
rename VotaciónTotal total

g municipality = "SAN MIGUEL TLACAMAMA EXTRAORDINARIO"
g uniqueid = 20285

collapse (sum) PRI_PVEM PUP listanominal total, by(municipality uniqueid section)

egen valid=rowtotal(PRI_PVEM PUP)

foreach var in PRI_PVEM PUP total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PRI_PVEM i_PUP, gen(PRI_PVEM_r PUP_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI_PVEM PUP {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

gen year=2014
gen month="May"
gen STATE="OAXACA"

order STATE municipality section uniqueid *

save Oaxaca_Section_2014.dta , replace

//////////////////////////////////
////////////2016//////////////////
//////////////////////////////////

import excel using "Concejales_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
  clear
  local sheetname=r(worksheet_`sheet')
  import excel "Concejales_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
  drop if DTTO=="" | DTTO=="DTTO"
  save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H

clear
append using "D01.dta"
append using "D02.dta"
append using "D03.dta"
append using "D04.dta"
append using "D05.dta"
append using "D06.dta"
append using "D07.dta"
append using "D08.dta"
append using "D09.dta"
append using "D11.dta"
append using "D12.dta"
append using "D13.dta"
append using "D14.dta"
append using "D15.dta"
append using "D16.dta"
append using "D17.dta"
append using "D18.dta"
append using "D19.dta"
append using "D20.dta"
append using "D21.dta"
append using "D22.dta"
append using "D23.dta"
append using "D24.dta"
append using "D25.dta"

drop LOCALIDAD DTTO AE CVE

destring *, replace

rename (PMC PNA PANPRD PRIPVEM PNAPSD) (MC PANAL PAN_PRD PRI_PVEM PANAL_PSD)
rename (MUNICIPIO SECCION LISTA_NOM CNR) (municipality section listanominal no_reg)
rename IND_* CI_*

replace PAN_PRD = PAN_PRD + PAN + PRD if PAN_PRD!=.
replace PAN = . if PAN_PRD!=.
replace PRD = . if PAN_PRD!=.

replace PRI_PVEM = PRI_PVEM + PRI + PVEM if PRI_PVEM!=.
replace PRI = . if PRI_PVEM!=.
replace PVEM = . if PRI_PVEM!=.

replace PANAL_PSD = PANAL_PSD + PANAL + PSD if PANAL_PSD!=.
replace PANAL = . if PANAL_PSD!=.
replace PSD = . if PANAL_PSD!=.

drop V
order PANAL_PSD, a(PRI_PVEM)
rename VOT_TOTAL total

drop if total==.

collapse (sum) total listanominal PAN-CI_5, by (municipality section)

preserve
import excel "uniqueids.xlsx", first sheet("2016") clear
save uniqueids16.dta, replace
restore

merge m:1 municipality using uniqueids16.dta
drop _merge municipality
erase uniqueids16.dta

rename municipality2 municipality
order municipality uniqueid section *

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PUP PANAL PSD MORENA PES PRS PAN_PRD PRI_PVEM PANAL_PSD CI_1 CI_2 CI_3 CI_4 CI_5)

foreach var in PAN PRI PRD PVEM PT MC PUP PANAL PSD MORENA PES PRS PAN_PRD PRI_PVEM PANAL_PSD CI_1 CI_2 CI_3 CI_4 CI_5 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

gen turnout = total/listanominal
gen mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_PUP i_PSD i_MORENA i_PES i_PRS i_MC i_PANAL i_PAN_PRD i_PRI_PVEM i_PANAL_PSD i_CI_1 i_CI_2 i_CI_3 i_CI_4 i_CI_5, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r PUP_r PSD_r MORENA_r PES_r PRS_r MC_r PANAL_r PAN_PRD_r PRI_PVEM_r PANAL_PSD_r CI_1_r CI_2_r CI_3_r CI_4_r CI_5_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT PUP PSD MORENA PES PRS MC PANAL PAN_PRD PRI_PVEM PANAL_PSD CI_1 CI_2 CI_3 CI_4 CI_5 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

gen year=2016
gen month="June"
gen STATE="OAXACA"

save Oaxaca_Section_2016.dta , replace

collapse (first) winner, by (municipality uniqueid)
replace winner = "" if uniqueid==20441 /* not clear who the incumbent was because the special election (below) was later annulled */
rename winner incumbent

save incumbents18.dta, replace


**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Oaxaca_Section_2016.dta 
collapse (sum) total-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Oaxaca_2016.dta", replace

erase "D01.dta"
erase "D02.dta"
erase "D03.dta"
erase "D04.dta"
erase "D05.dta"
erase "D06.dta"
erase "D07.dta"
erase "D08.dta"
erase "D09.dta"
erase "D11.dta"
erase "D12.dta"
erase "D13.dta"
erase "D14.dta"
erase "D15.dta"
erase "D16.dta"
erase "D17.dta"
erase "D18.dta"
erase "D19.dta"
erase "D20.dta"
erase "D21.dta"
erase "D22.dta"
erase "D23.dta"
erase "D24.dta"
erase "D25.dta"

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2017//////////////////
//////////////////////////////////

import excel "EXTRAORDINARIO_2017.xlsx", sheet("excel") firstrow clear

split CASILLA, p("-")
rename CASILLA1 section
rename L_NOM listanominal
rename Votos total

g municipality = "SANTA MARIA XADANI EXTRAORDINARIO"
g uniqueid = 20441

destring *, replace

replace PRI = PRI + PVEM + COALICION
rename PRI PRI_PVEM
drop PVEM COALICION

collapse (sum) PAN-PRS listanominal total, by(municipality uniqueid section)

egen valid=rowtotal(PAN PRI_PVEM PRD PT PMC PUP MORENA PRS)

foreach var in PAN PRI_PVEM PRD PT PMC PUP MORENA PRS total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI_PVEM i_PRD i_PT i_PMC i_PUP i_MORENA i_PRS, gen(PAN_r PRI_PVEM_r PRD_r PT_r PMC_r PUP_r MORENA_r PRS_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM PRD PT PMC PUP MORENA PRS {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}

drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"  | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2017
gen month="June"
gen STATE="OAXACA"

order STATE municipality section uniqueid *

save Oaxaca_Section_2017.dta , replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2018//////////////////
//////////////////////////////////

import excel using "Concejales_2018.xlsx", first clear

* Coalitions and communal candidates taken from the state electoral institute website
g coalition_pan_prd_mc = municipality!="LOMA BONITA" & municipality!="SAN FELIPE USILA" & municipality!="SAN JUAN IHUALTEPEC" & municipality!="SAN JUAN CACAHUATEPEC" & ///
	municipality!="SANTA MARIA XADANI" & municipality!="EL ESPINAL" & municipality!="SAN DIONISIO DEL MAR"
g coalition_pri_pvem_panal = municipality!="SAN JOSE TENANGO" & municipality!="SAN MIGUEL SOYALTEPEC" & municipality!="SAN JUAN BAUTISTA TUXTEPEC" & municipality!="SAN FELIPE JALAPA DE DIAZ" & ///
	municipality!="SAN JOSE CHILTEPEC" & municipality!="SAN JUAN BAUTISTA TLACOATZINTEPEC" & municipality!="SANTA MARIA JACATEPEC" & municipality!="HUAUTEPEC" & ///
	municipality!="HUAUTLA DE JIMENEZ" & municipality!="SAN BARTOLOME AYAUTLA" & municipality!="SAN JUAN BAUTISTA CUICATLAN" & municipality!="SAN JUAN COATZOSPAM" & ///
	municipality!="SANTA MARIA TECOMAVACA" & municipality!="SAN FRANCISCO TELIXTLAHUACA" & municipality!="VILLA DE TAMAZULAPAM DEL PROGRESO" & municipality!="FRESNILLO DE TRUJANO" & ///
	municipality!="HEROICA CIUDAD DE HUAJUAPAN DE LEON" & municipality!="MARISCALA DE JUAREZ" & municipality!="SAN JUAN IHUALTEPEC" & municipality!="SAN MARCOS ARTEAGA" & ///
	municipality!="SAN MIGUEL AMATITLAN" & municipality!="SANTIAGO HUAJOLOTITLAN" & municipality!="ZAPOTITLAN LAGUNAS" & municipality!="SAN JUAN CACAHUATEPEC" & ///
	municipality!="SAN JUAN CACAHUATEPEC" & municipality!="CHALCATONGO DE HIDALGO" & municipality!="CHALCATONGO DE HIDALGO" & municipality!="SAN JUAN BAUTISTA VALLE NACIONAL" & ///
	municipality!="CHAHUITES" & municipality!="MATIAS ROMERO AVENDAÑO" & municipality!="REFORMA DE PINEDA" & municipality!="SAN JUAN GUICHICOVI" & municipality!="SANTA MARIA PETAPA" & ///
	municipality!="SANTA MARIA PETAPA" & municipality!="SANTO DOMINGO INGENIO" & municipality!="SANTO DOMINGO ZANATEPEC" & municipality!="SANTA CRUZ AMILPAS" & ///
	municipality!="SANTA LUCIA DEL CAMINO" & municipality!="SOLEDAD ETLA" & municipality!="OAXACA DE JUAREZ" & municipality!="SANTA CRUZ XOXOCOTLAN" & municipality!="ASUNCION OCOTLAN" & ///
	municipality!="CIENEGA DE ZIMATLAN" & municipality!="SAN ANTONINO CASTILLO VELASCO" & municipality!="SANTA ANA ZEGACHE" & municipality!="SANTA GERTRUDIS" & ///
	municipality!="TRINIDAD ZAACHILA" & municipality!="ZIMATLAN DE ALVAREZ" & municipality!="SAN PABLO VILLA DE MITLA" & municipality!="SANTA MARIA JALAPA DEL MARQUES" & ///
	municipality!="SANTA MARIA MIXTEQUILLA" & municipality!="SANTO DOMINGO CHIHUITAN" & municipality!="SANTO DOMINGO PETAPA" & municipality!="SANTO DOMINGO TEHUANTEPEC" & ///
	municipality!="ASUNCION IXTALTEPEC" & municipality!="SALINA CRUZ" & municipality!="SAN BLAS ATEMPA" & municipality!="EL ESPINAL" & municipality!="SAN DIONISIO DEL MAR" & ///
	municipality!="SAN FRANCISCO DEL MAR" & municipality!="HEROICA CIUDAD DE EJUTLA DE CRESPO" & municipality!="MARTIRES DE TACUBAYA" & municipality!="MARTIRES DE TACUBAYA" & ///
	municipality!="SAN ANDRES HUAXPALTEPEC" & municipality!="SAN ANDRES HUAXPALTEPEC" & municipality!="SAN JUAN BAUTISTA LO DE SOTO" & municipality!="SAN MIGUEL TLACAMAMA" & ///
	municipality!="SAN PEDRO ATOYAC" & municipality!="SAN PEDRO JICAYAN" & municipality!="SAN SEBASTIAN IXCAPA" & municipality!="SANTA MARIA CORTIJO" & municipality!="SANTA MARIA HUAZOLOTITLAN" & ///
	municipality!="SANTIAGO TAPEXTLA" & municipality!="SAN PEDRO MIXTEPEC" & municipality!="SANTIAGO TETEPEC" & municipality!="MIAHUATLAN DE PORFIRIO DIAZ"
g coalition_pt_morena_pes = municipality!="SAN BARTOLOME AYAUTLA" & municipality!="SAN JUAN IHUALTEPEC" & municipality!="SAN DIONISIO DEL MAR" & ///
	municipality!="SAN JOSE ESTANCIA GRANDE" & municipality!="SAN MATEO RIO HONDO"
g cc_pan_prd_mc = municipality=="LOMA BONITA"
g cc_pri_pvem_panal = municipality=="SAN MIGUEL SOYALTEPEC" | municipality=="SAN JUAN BAUTISTA CUICATLAN" | municipality=="ANTA MARIA TECOMAVACA" | municipality=="FRESNILLO DE TRUJANO" | ///
	municipality=="SAN JUAN CACAHUATEPEC" | municipality=="SAN JUAN GUICHICOVI" | municipality=="SANTO DOMINGO INGENIO" | municipality=="SANTO DOMINGO ZANATEPEC" | ///
	municipality=="SANTA GERTRUDIS" | municipality=="SAN BLAS ATEMPA" | municipality=="EL ESPINAL" | municipality=="MARTIRES DE TACUBAYA" | municipality=="SAN SEBASTIAN IXCAPA"
g cc_pri_pvem = municipality=="SAN JOSE CHILTEPEC" | municipality=="SAN FRANCISCO TELIXTLAHUACA" | municipality=="SAN MIGUEL AMATITLAN" | municipality=="REFORMA DE PINEDA" | ///
	municipality=="SANTA LUCIA DEL CAMINO" | municipality=="SOLEDAD ETLA" | municipality=="OAXACA DE JUAREZ" | municipality=="SANTO DOMINGO PETAPA" | municipality=="ASUNCION IXTALTEPEC" | ///
	municipality=="SALINA CRUZ" | municipality=="SAN PEDRO MIXTEPEC" | municipality=="MIAHUATLAN DE PORFIRIO DIAZ"
g cc_pri_panal = municipality=="SAN JOSE TENANGO" | municipality=="SAN FELIPE JALAPA DE DIAZ" | municipality=="HUAUTLA DE JIMENEZ" | municipality=="SAN JUAN COATZOSPAM" | ///
	municipality=="MATIAS ROMERO AVENDAÑO" | municipality=="TRINIDAD ZAACHILA" | municipality=="ZIMATLAN DE ALVAREZ" | municipality=="SANTA MARIA JALAPA DEL MARQUES" | ///
	municipality=="SANTA MARIA MIXTEQUILLA" | municipality=="SANTO DOMINGO TEHUANTEPEC" | municipality=="HEROICA CIUDAD DE EJUTLA DE CRESPO"
g cc_pvem_panal = municipality=="SANTA MARIA PETAPA" | municipality=="SAN ANTONINO CASTILLO VELASCO" | municipality=="SANTA ANA ZEGACHE" | municipality=="SANTO DOMINGO CHIHUITAN" | ///
	municipality=="PINOTEPA DE DON LUIS"
g cc_prd_mc = municipality=="SAN FELIPE USILA"
g cc_prd_mc_pup = municipality=="EL ESPINAL"

replace coalition_pri_pvem_panal = 1 if cc_pri_pvem_panal==1
replace coalition_pan_prd_mc = 1 if cc_pan_prd_mc==1
drop cc_pri_pvem_panal cc_pan_prd_mc

egen PAN_PRD_MC = rowtotal(PAN PRD MC COAL_PAN_PRD_MC CC_PAN_PRD_MC) if coalition_pan_prd_mc==1
replace PAN = . if coalition_pan_prd_mc==1
replace PRD = . if coalition_pan_prd_mc==1
replace MC = . if coalition_pan_prd_mc==1

egen PRI_PVEM_PANAL = rowtotal(PRI PVEM PANAL COAL_PRI_PVEM_PANAL CC_PRI_PVEM_NA CC_PRI_PVEM CC_PRI_NA CC_PVEM_NA) if coalition_pri_pvem_panal==1
replace PRI = . if coalition_pri_pvem_panal==1
replace PVEM = . if coalition_pri_pvem_panal==1
replace PANAL = . if coalition_pri_pvem_panal==1

egen PRI_PVEM = rowtotal(PRI PVEM CC_PRI_PVEM) if cc_pri_pvem==1
replace PRI = . if cc_pri_pvem==1
replace PVEM = . if cc_pri_pvem==1

egen PRI_PANAL = rowtotal(PRI PANAL CC_PRI_NA) if cc_pri_panal==1
replace PRI = . if cc_pri_panal==1
replace PANAL = . if cc_pri_panal==1

egen PVEM_PANAL = rowtotal(PANAL PVEM CC_PVEM_NA) if cc_pvem_panal==1
replace PANAL = . if cc_pvem_panal==1
replace PVEM = . if cc_pvem_panal==1

egen PT_MORENA_PES = rowtotal(PT MORENA PES COAL_PT_MORENA_PES) if coalition_pt_morena_pes==1
drop PT MORENA PES

egen PRD_MC_PUP = rowtotal(PRD MC PUP CC_PRD_MC_PUP) if cc_prd_mc_pup==1
replace PRD = . if cc_prd_mc_pup==1
replace MC = . if cc_prd_mc_pup==1
replace PUP = . if cc_prd_mc_pup==1

egen PRD_MC = rowtotal(PRD MC CC_PRD_MC) if cc_prd_mc==1
replace PRD = . if cc_prd_mc==1
replace MC = . if cc_prd_mc==1

drop CC* COAL*
order PAN_PRD_MC-PRD_MC, a(PMR)

drop municipality
rename munname_INEGI municipality
rename TOTAL total

egen CI_1=rowtotal(CIALBERTO-CIJOELA)
egen CI_2=rowtotal(CIELIAZAR CIIVAN CIADOLFOJ)
egen CI_3=rowtotal(CIRAUL CIGERMAN)
replace CI_1=CI_1-CI_2-CI_3
drop CIALBERTO-CIJOELA
order CI_*, a(PRD_MC)

collapse (sum) PAN-CI_3 listanominal total (first) cc* coalition*, by (municipality uniqueid section)

egen valid=rowtotal(PAN PRI PRD PVEM MC PUP PANAL PSD PMR PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA_PES PRD_MC_PUP PRI_PVEM PRI_PANAL PVEM_PANAL PRD_MC CI_1 CI_2 CI_3)

foreach var in PAN PRI PRD PVEM MC PUP PANAL PSD PMR PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA_PES PRD_MC_PUP PRI_PVEM PRI_PANAL PVEM_PANAL PRD_MC CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_MC i_PUP i_PANAL i_PSD i_PMR i_PAN_PRD_MC i_PRI_PVEM_PANAL i_PT_MORENA_PES i_PRD_MC_PUP i_PRI_PVEM i_PRI_PANAL i_PVEM_PANAL i_PRD_MC i_CI_1 i_CI_2 i_CI_3, ///
	gen(PAN_r PRI_r PRD_r PVEM_r MC_r PUP_r PANAL_r PSD_r PMR_r PAN_PRD_MC_r PRI_PVEM_PANAL_r PT_MORENA_PES_r PRD_MC_PUP_r PRI_PVEM_r PRI_PANAL_r PVEM_PANAL_r PRD_MC_r CI_1_r CI_2_r CI_3_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM MC PUP PANAL PSD PMR PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA_PES PRD_MC_PUP PRI_PVEM PRI_PANAL PVEM_PANAL PRD_MC CI_1 CI_2 CI_3  {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"  | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2018
gen month="July"
gen STATE="OAXACA"

order STATE municipality section uniqueid *

merge m:1 uniqueid using incumbents18.dta
drop _merge
erase incumbents18.dta
replace incumbent = "PRS" if municipality=="SANTA MARÍA XADANI"

g incumbent_vote=.

foreach var in PAN PRI PRD PANAL MC PUP PSD {
	replace incumbent_vote=`var' if incumbent=="`var'"
}

replace incumbent_vote = PAN_PRD_MC if (incumbent=="PAN" | incumbent=="PRD" | incumbent=="MC") & coalition_pan_prd_mc==1
replace incumbent_vote = PRI_PVEM_PANAL if (incumbent=="PRI" | incumbent=="PVEM" | incumbent=="PANAL") & coalition_pri_pvem_panal==1
replace incumbent_vote = PRI_PVEM if (incumbent=="PRI" | incumbent=="PVEM") & cc_pri_pvem==1
replace incumbent_vote = PRI_PANAL if (incumbent=="PRI" | incumbent=="PANAL") & cc_pri_panal==1
replace incumbent_vote = PVEM_PANAL if (incumbent=="PVEM" | incumbent=="PANAL") & cc_pvem_panal==1
replace incumbent_vote = PT_MORENA_PES if (incumbent=="PT" | incumbent=="MORENA" | incumbent=="PES") & coalition_pt_morena_pes==1
replace incumbent_vote = PRD_MC_PUP if (incumbent=="PRD" | incumbent=="MC" | incumbent=="PUP") & cc_prd_mc_pup==1
replace incumbent_vote = PRD_MC if (incumbent=="PRD" | incumbent=="MC") & cc_prd_mc==1
		
*Take max as incumbents listed as PRI-PVEM without party affiliation
egen maxpanprd=rowmax(PAN PRD PAN_PRD_MC)
replace incumbent_vote=maxpanprd if incumbent=="PAN_PRD"

egen maxpripvem=rowmax(PRI PVEM PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL)
replace incumbent_vote=maxpripvem if incumbent=="PRI_PVEM"

*Reforma de Pineda and Putla Villa de Guerrero, both independent municipal presidents ran for reelection
replace incumbent_vote=CI_1 if incumbent=="Independent"

drop max*
drop cc* coalition*

save Oaxaca_Section_2018.dta , replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Oaxaca_2018.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2018//////////////////
//////////////////////////////////

import excel "EXTRAORDINARIO_2018.xlsx", sheet("COMPAYU20020") cellrange(A3:AH5) firstrow clear

rename SECCION section
g municipality = "SAN JUAN IHUALTEPEC EXTRAORDINARIO"
g uniqueid = 20199
rename LISTA_NOMINAL_CASILLA listanominal
rename TOTAL_VOTOS total

replace PRI = PRI + PVEM + NA + CANDIDATURACOMÚNPRIPVEMNA + PRIPVEM + PRINA + PVEMNA
rename PRI PRI_PVEM_PANAL
drop PVEM NA CANDIDATURACOMÚNPRIPVEMNA PRIPVEM PRINA PVEMNA

collapse (sum) PAN-MORENA listanominal total, by(municipality uniqueid section)

egen valid = rowtotal(PAN PRI_PVEM_PANAL MORENA)

foreach var in PAN PRI_PVEM_PANAL MORENA total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI_PVEM_PANAL i_MORENA, gen(PAN_r PRI_PVEM_PANAL_r MORENA_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL MORENA {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"  | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2018
gen month="December"
gen STATE="OAXACA"

order STATE municipality section uniqueid *

save Oaxaca_Section_2018_EXTRAORDINARIO.dta , replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

clear
append using Oaxaca_Section_2013.dta
append using Oaxaca_Section_2014.dta
append using Oaxaca_Section_2016.dta
append using Oaxaca_Section_2017.dta
append using Oaxaca_Section_2018.dta
append using Oaxaca_Section_2018_EXTRAORDINARIO.dta

erase Oaxaca_Section_2013.dta
erase Oaxaca_Section_2014.dta
erase Oaxaca_Section_2016.dta
erase Oaxaca_Section_2017.dta
erase Oaxaca_Section_2018.dta
erase Oaxaca_Section_2018_EXTRAORDINARIO.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Oaxaca_Section_14_18.dta, replace

use "..\..\Precinct\Oaxaca_ALL.dta", clear
append using Oaxaca_Section_14_18.dta

erase Oaxaca_Section_14_18.dta

replace uniqueid = 20066 if municipality=="SANTIAGO NILTEPEC"
replace municipality = upper(municipality)

save Oaxaca_ALL_SALVADOR.dta, replace
