////////////////////////////////////////////
////COLIMA PRECINCT PANEL UPDATE//////////
////JUN 2019////////////////////////////////
////SALVADOR ASCENCIO///////////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Colima"

//////////////////////////////////
////////////2015//////////////////
//////////////////////////////////

*6001 ARMERIA
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("ARMERIA") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="ARMERIA"
gen uniqueid="6001"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save ARMERIA_PS.dta, replace

*6002 COLIMA
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("COLIMA") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="COLIMA"
gen uniqueid="6002"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save COLIMA_PS.dta, replace

*6003 COMALA
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("COMALA") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="COMALA"
gen uniqueid="6003"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save COMALA_PS.dta, replace

*6004 COQUIMATLAN
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("COQUIMATLAN") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="COQUIMATLAN"
gen uniqueid="6004"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save COQUIMATLAN_PS.dta, replace

*6005 CUAUHTEMOC
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("CUAUHTEMOC") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="CUAUHTEMOC"
gen uniqueid="6005"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save CUAUHTEMOC_PS.dta, replace

*6006 IXTLAHUACAN
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("IXTLAHUACAN") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="IXTLAHUACAN"
gen uniqueid="6006"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save IXTLAHUACAN_PS.dta, replace

*6007 MANZANILLO
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("MANZANILLO") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="MANZANILLO"
gen uniqueid="6007"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save MANZANILLO_PS.dta, replace

*6008 MINATITLAN
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("MINATITLAN") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="MINATITLAN"
gen uniqueid="6008"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save MINATITLAN_PS.dta, replace

*6009 TECOMAN
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("TECOMAN") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="TECOMAN"
gen uniqueid="6009"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save TECOMAN_PS.dta, replace

*6010 VILLA DE ALVAREZ
clear
import excel "RESULTADOS ELECTORALES DE AYUNTAMIENTO ELECCION 2014-2015.xls", sheet("VILLA DE ALVAREZ") clear firstrow allstring
drop if TIPOCASILLA ==""
replace SECCION = SECCION[_n-1] if SECCION =="" &  SECCION[_n-1]!=""
keep SECCION LISTANOMINAL - TOTAL
gen municipality ="VILLA DE ALVAREZ"
gen uniqueid="6010"
rename SECCION section
rename LISTANOMINAL listanominal
rename TOTAL total
rename CANDIDATOSNOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename Morena MORENA
destring *, replace
foreach x of varlist listanominal-total {
	replace `x' = 0 if `x'==.
}
collapse (sum) listanominal - total, by(municipality uniqueid section)

save VILLADEALVAREZ_PS.dta, replace

clear 

append using ARMERIA_PS.dta
append using COLIMA_PS.dta
append using COMALA_PS.dta
append using COQUIMATLAN_PS.dta
append using CUAUHTEMOC_PS.dta
append using IXTLAHUACAN_PS.dta
append using MANZANILLO_PS.dta
append using MINATITLAN_PS.dta
append using TECOMAN_PS.dta
append using VILLADEALVAREZ_PS.dta

erase ARMERIA_PS.dta
erase COLIMA_PS.dta
erase COMALA_PS.dta
erase COQUIMATLAN_PS.dta
erase CUAUHTEMOC_PS.dta
erase IXTLAHUACAN_PS.dta
erase MANZANILLO_PS.dta
erase MINATITLAN_PS.dta
erase TECOMAN_PS.dta
erase VILLADEALVAREZ_PS.dta

replace PES = PESE if  PES ==. &  PESE!=.
drop PESE

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI + PVEM + PANAL if PRI_PVEM_PANAL!=.
replace PRI = . if PRI_PVEM_PANAL!=.
replace PVEM = . if PRI_PVEM_PANAL!=.
replace PANAL = . if PRI_PVEM_PANAL!=.

order PANAL PRI_PVEM_PANAL, a(PES)

g year=2015
g month="June"

egen valid = rowtotal(PAN PRI PRD PVEM PT MC MORENA PH PES PANAL PRI_PVEM_PANAL)

foreach var in PAN PRI PRD PVEM PT MC MORENA PH PES PANAL PRI_PVEM_PANAL total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

gen turnout=total/listanominal
gen mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_MORENA i_PH i_PES i_PANAL i_PRI_PVEM_PANAL, gen( PAN_r PRI_r PRD_r PVEM_r PT_r MC_r MORENA_r PH_r PES_r PANAL_r PRI_PVEM_PANAL_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC MORENA PH PES PANAL PRI_PVEM_PANAL {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

drop nulo no_reg 

g STATE="COLIMA"

save COLIMA_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent
save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use COLIMA_2015.dta, clear
collapse (sum) listanominal-PRI_PVEM_PANAL valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Colima_2015.dta", replace

***********************************************************************************************************************
***********************************************************************************************************************

//////////////////////////////////
////////////2018//////////////////
//////////////////////////////////

import excel "1 COL RES AYUN.xlsx", sheet("Reporte_Casillas_Ayuntamientos") clear firstrow allstring

rename Municipio municipality
rename Sección section

rename ListaNominal listanominal
rename NOREG no_reg
rename NULOS nulo
rename TOTALDEVOTOS total

destring *, replace

gen PAN_PRD = PAN + PRD + PANPRD	
drop PAN PRD PANPRD
 
gen PRI_PVEM_PANAL = PRI + PVEM + PRIPVEM	
drop PRI  PVEM  PRIPVEM	

gen MORENA_PT_PES =  PTMORENAPES + PTMORENA + MORENAPES	+ PTPES	+  PT + MORENA + PES
drop PTMORENAPES  PTMORENA  MORENAPES PTPES	 PT  MORENA  PES

gen CI_1 = MUNICIPIOCOMALA + MUNICIPIOMANZANILLO
drop MUNICIPIOCOMALA MUNICIPIOMANZANILLO

rename PNA PANAL

order PAN_PRD PRI_PVEM_PANAL MORENA_PT_PES CI_1, a(PANAL)

collapse (sum) listanominal MC-CI_1 total, by (municipality section uniqueid)

egen valid = rowtotal (MC PANAL PAN_PRD PRI_PVEM_PANAL MORENA_PT_PES CI_1)

foreach var in MC PANAL PAN_PRD PRI_PVEM_PANAL MORENA_PT_PES CI_1 total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN_PRD i_PRI_PVEM_PANAL i_MORENA_PT_PES i_MC i_PANAL i_CI_1, gen( PAN_PRD_r PRI_PVEM_PANAL_r MORENA_PT_PES_r MC_r PANAL_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in MC PANAL PAN_PRD PRI_PVEM_PANAL MORENA_PT_PES CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1"
replace second="Independent" if second=="CI_1"
replace third="Independent" if third=="CI_1"

gen year=2018
gen month="July"
gen STATE="COLIMA"

save COLIMA_2018.dta, replace

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta

g incumbent_vote = .

replace incumbent_vote=PAN_PRD if incumbent=="PAN"
replace incumbent_vote=PRI_PVEM if incumbent=="PRI" | incumbent=="PVEM"

save COLIMA_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use COLIMA_2018.dta, clear
collapse (sum) listanominal-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Colima_2018.dta", replace

***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************

use COLIMA_2015.dta, clear
append using COLIMA_2018.dta

erase COLIMA_2015.dta
erase COLIMA_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 & (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT MC PVEM PANAL MORENA {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
drop winner_counter

save COLIMA_Section_15_18.dta, replace

use "..\..\Precinct\COLIMA_ALL.dta", clear
append using COLIMA_Section_15_18.dta

erase COLIMA_Section_15_18.dta

save COLIMA_ALL_SALVADOR.dta, replace

