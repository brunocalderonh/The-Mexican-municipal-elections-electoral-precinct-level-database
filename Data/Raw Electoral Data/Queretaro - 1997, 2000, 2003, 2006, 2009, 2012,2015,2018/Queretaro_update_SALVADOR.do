////////////////////////////////////////////
////////QUERETARO PRECINCT PANEL UPDATE/////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

clear
cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Queretaro"

import excel using "Municipios_2015.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Municipios_2015.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipality="`sheetname'"
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "AMEALCO DE BONFIL.dta"
append using "ARROYO SECO.dta"
append using "CADEREYTA DE MONTES.dta"
append using "COLÓN.dta"
append using "CORREGIDORA.dta"
append using "EL MARQUÉS.dta"
append using "EZEQUIEL MONTES.dta"
append using "HUIMILPAN.dta"
append using "JALPAN DE SERRA.dta"
append using "LANDA DE MATAMOROS.dta"
append using "PEDRO ESCOBEDO.dta"
append using "PEÑAMILLER.dta"
append using "PINAL DE AMOLES.dta"
append using "QUERÉTARO.dta"
append using "SAN JOAQUÍN.dta"
append using "SAN JUAN DEL RÍO.dta"
append using "TEQUISQUIAPAN.dta"
append using "TOLIMÁN.dta"

drop if SECCIÓN==""

destring *, replace

gen uniqueid=.
replace uniqueid=22001 if municipality== "AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality== "ARROYO SECO"
replace uniqueid=22004 if municipality== "CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality== "COLÓN"
replace uniqueid=22006 if municipality== "CORREGIDORA"
replace uniqueid=22011 if municipality== "EL MARQUÉS"
replace uniqueid=22007 if municipality== "EZEQUIEL MONTES"
replace uniqueid=22008 if municipality== "HUIMILPAN"
replace uniqueid=22009 if municipality== "JALPAN DE SERRA"
replace uniqueid=22010 if municipality== "LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality== "PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality== "PEÑAMILLER"
replace uniqueid=22002 if municipality== "PINAL DE AMOLES"
replace uniqueid=22014 if municipality== "QUERÉTARO"
replace uniqueid=22015 if municipality== "SAN JOAQUÍN"
replace uniqueid=22016 if municipality== "SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality== "TEQUISQUIAPAN"
replace uniqueid=22018 if municipality== "TOLIMÁN"

rename SECCIÓN section
order PVEM_PANAL-PRI_PVEM_PANAL_PT, a(PRI_PT)

rename PANAL_PT PT_PANAL

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI + PANAL + PVEM + PVEM_PANAL + PRI_PANAL + PRI_PVEM if PRI_PVEM_PANAL!=.
replace PRI = . if PRI_PVEM_PANAL!=.
replace PANAL = . if PRI_PVEM_PANAL!=.
replace PVEM = . if PRI_PVEM_PANAL!=.
replace PVEM_PANAL = . if PRI_PVEM_PANAL!=.
replace PRI_PANAL = . if PRI_PVEM_PANAL!=.
replace PRI_PVEM = . if PRI_PVEM_PANAL!=.

replace PRI_PT_PANAL = PRI_PT_PANAL + PRI + PANAL + PT + PT_PANAL + PRI_PANAL + PRI_PT if PRI_PT_PANAL!=.
replace PRI = . if PRI_PT_PANAL!=.
replace PANAL = . if PRI_PT_PANAL!=.
replace PT = . if PRI_PT_PANAL!=.
replace PRI_PANAL = . if PRI_PT_PANAL!=.
replace PT_PANAL = . if PRI_PT_PANAL!=.
replace PRI_PT = . if PRI_PT_PANAL!=.

replace PVEM_PANAL = PANAL + PVEM + PVEM_PANAL if PRI_PVEM_PANAL==. & PVEM_PANAL!=.
replace PANAL = . if PRI_PVEM_PANAL==. & PVEM_PANAL!=.
replace PVEM = . if PRI_PVEM_PANAL==. & PVEM_PANAL!=.

replace PAN_PRD = PRD + PAN + PAN_PRD if PAN_PRD!=.
replace PRD = . if PAN_PRD!=.
replace PAN = . if PAN_PRD!=.

replace PRI_PT = PT + PRI + PRI_PT if PRI_PT_PANAL==. & PRI_PT!=.
replace PT = . if PRI_PT_PANAL==. & PRI_PT!=.
replace PRI = . if PRI_PT_PANAL==. & PRI_PT!=.

replace PRI_PVEM = PVEM + PRI + PRI_PVEM if PRI_PVEM_PANAL==. & PRI_PVEM!=.
replace PVEM = . if PRI_PVEM_PANAL==. & PRI_PVEM!=.
replace PRI = . if PRI_PVEM_PANAL==. & PRI_PVEM!=.

drop PRI_PANAL PT_PANAL

egen total = rowtotal(PAN-Nulos)

collapse (sum) PAN-PRI_PVEM_PANAL_PT total, by (municipality section uniqueid)

egen valid=rowtotal(PAN PRI PRD MC PANAL PVEM PES MORENA PT PRI_PT PVEM_PANAL PH CI_1 PRI_PVEM_PANAL PRI_PVEM PRI_PT_PANAL CI_2 PAN_PRD PRI_PVEM_PANAL_PT)

foreach var in PAN PRI PRD MC PANAL PVEM PES MORENA PT PRI_PT PVEM_PANAL PH CI_1 PRI_PVEM_PANAL PRI_PVEM PRI_PT_PANAL CI_2 PAN_PRD PRI_PVEM_PANAL_PT total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_MC i_PANAL i_PVEM i_PES i_MORENA i_PT i_PRI_PT i_PVEM_PANAL i_PH i_CI_1 i_PRI_PVEM_PANAL i_PRI_PVEM i_PRI_PT_PANAL i_CI_2 i_PAN_PRD i_PRI_PVEM_PANAL_PT, ///
	gen(PAN_r PRI_r PRD_r MC_r PANAL_r PVEM_r PES_r MORENA_r PT_r PRI_PT_r PVEM_PANAL_r PH_r CI_1_r PRI_PVEM_PANAL_r PRI_PVEM_r PRI_PT_PANAL_r CI_2_r PAN_PRD_r PRI_PVEM_PANAL_PT_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD MC PANAL PVEM PES MORENA PT PRI_PT PVEM_PANAL PH CI_1 PRI_PVEM_PANAL PRI_PVEM PRI_PT_PANAL CI_2 PAN_PRD PRI_PVEM_PANAL_PT {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" |  winner=="CI_2" 
replace second="Independent" if second=="CI_1"  | second=="CI_2"  
replace third="Independent" if third=="CI_1" | third=="CI_2"

gen year=2015 
gen month="June"
gen STATE="QUERETARO"
****The ordinary election  in Huimilpan was annuled/ an extraordinary election was held in december 2015. The results of the extraordinary election are incorporated
*replace month="December" if uniqueid==22008
drop if uniqueid==22008

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election (June 30 2015) 
keep entidad municipio seccion lista file year month
keep if entidad==22 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_QUER.dta, replace
restore

merge 1:1 section using LN15_QUER.dta
drop if _merge==2
drop _merge
erase LN15_QUER.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

save Queretaro_Section_2015.dta, replace 

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent

save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Queretaro_Section_2015.dta
collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Queretaro_2015.dta", replace

**********************************************************************
**********************************************************************

erase "AMEALCO DE BONFIL.dta"
erase "ARROYO SECO.dta"
erase "CADEREYTA DE MONTES.dta"
erase "COLÓN.dta"
erase "CORREGIDORA.dta"
erase "EL MARQUÉS.dta"
erase "EZEQUIEL MONTES.dta"
erase "HUIMILPAN.dta"
erase "JALPAN DE SERRA.dta"
erase "LANDA DE MATAMOROS.dta"
erase "PEDRO ESCOBEDO.dta"
erase "PEÑAMILLER.dta"
erase "PINAL DE AMOLES.dta"
erase "QUERÉTARO.dta"
erase "SAN JOAQUÍN.dta"
erase "SAN JUAN DEL RÍO.dta"
erase "TEQUISQUIAPAN.dta"
erase "TOLIMÁN.dta"


********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

//////////////////////////////////////
////////2015 EXTRAORDINARIO///////////
//////////////////////////////////////

import delimited "D:\Dropbox\Salvador Ascencio\Update Precincts\Queretaro\huimilpan.csv", clear

rename seccion section
drop if pri==.
destring _all, replace

g PRI_PVEM_PANAL = pri + panal + pvem + pripanalpvem + pripanal + pripvem + panalpvem
g PAN_PRD = pan + prd + panprd
drop pri panal pvem pripanalpvem pripanal pripvem panalpvem pan prd panprd
rename pes PES
rename morena MORENA
rename pt PT

g uniqueid = 22008 
g municipality = "HUIMILPAN EXTRAORDINARIO"

egen total = rowtotal(PES MORENA PT PRI_PVEM_PANAL PAN_PRD nr nulos)

collapse (sum) PES MORENA PT PRI_PVEM_PANAL PAN_PRD total, by (municipality section uniqueid)

egen valid = rowtotal(PES MORENA PT PRI_PVEM_PANAL PAN_PRD)

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
keep if entidad==22 & month==11 & year==2015
rename seccion section
rename lista listanominal
save merge.dta, replace
restore

merge 1:1 section using merge.dta, keepusing(listanominal)
drop if _merge==2
drop _merge
erase merge.dta

foreach var in PES MORENA PT PRI_PVEM_PANAL PAN_PRD total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PES i_MORENA i_PT i_PRI_PVEM_PANAL i_PAN_PRD, gen(PES_r MORENA_r PT_r PRI_PVEM_PANAL_r PAN_PRD_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PES MORENA PT PRI_PVEM_PANAL PAN_PRD {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

gen year=2015
gen month="December"
gen STATE="QUERETARO"

save Queretaro_Section_2015_EXTRAORDINARIO.dta, replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

import excel "Municipios_2018.xlsx", sheet("Municipios") clear firstrow 

replace MORENA_PT_PES = MORENA_PT_PES + MORENA_PT + MORENA_PES + PT_PES + MORENA + PT + PES if MORENA_PT_PES!=.
replace PT =. if  MORENA_PT_PES!=.
replace PES =. if  MORENA_PT_PES!=.
replace MORENA = . if  MORENA_PT_PES!=.
drop MORENA_PT MORENA_PES PT_PES

replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if PAN_PRD_MC!=.
replace PAN=. if PAN_PRD_MC!=.
replace PRD=. if PAN_PRD_MC!=.
replace MC=. if PAN_PRD_MC!=.
drop PRD_MC PAN_PRD PAN_MC

g PAN_PRD = CC2_PANPRD + PAN + PRD if CC2_PANPRD!=.
replace PAN=. if CC2_PANPRD!=.
replace PRD=. if CC2_PANPRD!=.
drop CC2_PANPRD

g PAN_MC = CC3_PANMC + PAN + MC if CC3_PANMC!=.
replace PAN=. if CC3_PANMC!=.
replace MC=. if CC3_PANMC!=.
drop CC3_PANMC

replace PRI_PVEM = PRI_PVEM + PRI + PVEM if PRI_PVEM!=.
replace PRI=. if PRI_PVEM!=.
replace PVEM=. if PRI_PVEM!=.

replace PRI_PVEM = CC4_PRIPVEM + PRI + PVEM if CC4_PRIPVEM!=.
replace PRI=. if CC4_PRIPVEM!=.
replace PVEM=. if CC4_PRIPVEM!=.
drop CC4_PRIPVEM

egen CI_1 = rowtotal(JAGRV LGOD LBH OELO MGAG RMH EMB JANH JNL JMMM JAML PVM ACM DJD JBLL HMV RMS JLMS PRCL AMG RRT AGAB EFM)
gen CI_2 = JNL + JANH + PRCL + ACM + AMG
gen CI_3 = JMMM + JLMS + AGAB
gen CI_4 = EFM
replace CI_1 = CI_1 - CI_2 - CI_3 - CI_4
drop JAGRV LGOD LBH OELO MGAG RMH EMB JANH JNL JMMM JAML PVM ACM DJD JBLL HMV RMS JLMS PRCL AMG RRT AGAB EFM

order PAN_PRD-CI_4, a(PAN_PRD_MC)

drop uniqueid
gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"

collapse (sum) PAN-CI_4 listanominal total, by (municipality section uniqueid)

egen valid = rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PES CQ QI MORENA_PT_PES PRI_PVEM PAN_PRD_MC PAN_PRD PAN_MC CI_1 CI_2 CI_3 CI_4)

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES CQ QI MORENA_PT_PES PRI_PVEM PAN_PRD_MC PAN_PRD PAN_MC CI_1 CI_2 CI_3 CI_4 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_CQ i_QI i_MORENA_PT_PES i_PRI_PVEM i_PAN_PRD_MC i_PAN_PRD i_PAN_MC i_CI_1 i_CI_2 i_CI_3 i_CI_4, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r CQ_r QI_r MORENA_PT_PES_r PRI_PVEM_r PAN_PRD_MC_r PAN_PRD_r PAN_MC_r CI_1_r CI_2_r CI_3_r CI_4_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES CQ QI MORENA_PT_PES PRI_PVEM PAN_PRD_MC PAN_PRD PAN_MC CI_1 CI_2 CI_3 CI_4 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" |  winner=="CI_2" 
replace second="Independent" if second=="CI_1"  | second=="CI_2"  
replace third="Independent" if third=="CI_1" | third=="CI_2"

gen year=2018
gen month="July"
gen STATE="QUERETARO"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta
replace incumbent = "PRI_PVEM_PANAL" if municipality=="HUIMILPAN"

gen incumbent_vote = .

egen maxpan=rowmax(PAN_PRD_MC PAN_PRD PAN_MC PAN)
replace incumbent_vote=maxpan if incumbent=="PAN" 

replace incumbent_vote=PANAL if incumbent=="PANAL"

egen maxpri=rowmax(PRI PRI_PVEM)
replace incumbent_vote=maxpri if strpos(incumbent, "PRI")>0

replace incumbent_vote=PVEM if uniqueid==22017

drop max*

save Queretaro_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) listanominal-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Queretaro_2018.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

clear
append using Queretaro_Section_2015.dta
append using Queretaro_Section_2015_EXTRAORDINARIO.dta
append using Queretaro_Section_2018.dta

erase Queretaro_Section_2015.dta
erase Queretaro_Section_2015_EXTRAORDINARIO.dta
erase Queretaro_Section_2018.dta

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

save Queretaro_Section_15_18.dta, replace

use "..\..\Precinct\Queretaro_ALL.dta", clear
append using Queretaro_Section_15_18.dta

erase Queretaro_Section_15_18.dta

replace municipality = upper(municipality)

save Queretaro_ALL_SALVADOR.dta, replace 
