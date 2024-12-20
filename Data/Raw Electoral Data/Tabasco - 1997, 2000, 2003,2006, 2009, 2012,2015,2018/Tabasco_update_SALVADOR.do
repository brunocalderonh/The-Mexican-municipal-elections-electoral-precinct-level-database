////////////////////////////////////////////
////////TABASCO PRECINCT PANEL UPDATE///////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Tabasco"

import excel "Ayuntamientos_2015.xlsx", sheet("DESGLOSE") clear firstrow

destring section, replace

drop uniqueid
gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MENDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAISO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

replace municipality = "CENTRO EXTRAORDINARIO" if municipality=="CENTRO"

egen CI_1= rowtotal(CIND CI1 CI2 CI3 CI4 CI5) 
drop CIND CI1 CI2 CI3 CI4 CI5
order CI_1, a(PVEM_PANAL)

replace PRD_PANAL=PRD+PANAL+PRD_PANAL if PRD_PANAL!=.
replace PRD=. if PRD_PANAL!=.
replace PANAL=. if PRD_PANAL!=.

replace PRD_PT=PRD+PT+PRD_PT if PRD_PT!=.
replace PRD=. if PRD_PT!=.
replace PT=. if PRD_PT!=.

replace PRI_PVEM_PANAL=PRI+PVEM+PANAL+PRI_PVEM+PRI_PANAL+PVEM_PANAL+PRI_PVEM_PANAL if PRI_PVEM_PANAL!=.
replace PRI=. if PRI_PVEM_PANAL!=.
replace PVEM=. if PRI_PVEM_PANAL!=.
replace PANAL=. if PRI_PVEM_PANAL!=.
drop PRI_PVEM PRI_PANAL PVEM_PANAL

collapse (sum) PAN-CI_1 total listanominal, by (municipality uniqueid section)

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRD_PANAL PRD_PT PRI_PVEM_PANAL CI_1) 

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election in CENTRO (March 13 2016) 
keep entidad municipio seccion lista file year month
gen uniqueid=(entidad*1000)+ municipio
keep if uniqueid==27004 & month==2
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN16_TAB.dta, replace
restore

merge 1:1 section using LN16_TAB.dta
drop if _merge==2
drop _merge
erase LN16_TAB.dta

replace listanominal = lista if uniqueid==27004
drop lista

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRD_PANAL PRD_PT PRI_PVEM_PANAL CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PH i_PES i_PRD_PANAL i_PRD_PT i_PRI_PVEM_PANAL i_CI_1, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PH_r PES_r PRD_PANAL_r PRD_PT_r PRI_PVEM_PANAL_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRD_PANAL PRD_PT PRI_PVEM_PANAL CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

gen year=2015
replace year=2016 if uniqueid==27004
gen month="June"
replace month="March" if uniqueid==27004
gen STATE="TABASCO"

destring section, replace force

save Tabasco_Section_2015.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Tabasco_Section_2015.dta
collapse (sum) PAN-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Tabasco_2015.dta", replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

import excel "Ayuntamientos_2018.xlsx", sheet("1 TAB RES AYUN") clear firstrow 

drop uniqueid
gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MENDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAISO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

replace PAN_PRD_MC = PAN_PRD_MC + PAN + PRD + MC + PAN_PRD + PAN_MC + PRD_MC
drop PAN PRD MC PAN_PRD PAN_MC PRD_MC

replace PT_MORENA= PT_MORENA + PT + MORENA if PT_MORENA!=.
replace PT=. if PT_MORENA!=.
replace MORENA=. if PT_MORENA!=.

egen CI_1=rowtotal(CAND_IND10-CAND_IND17)
drop CAND*

order CI_1, a(PT_MORENA)

collapse (sum) PRI-CI_1 total listanominal, by (municipality uniqueid section)

egen valid=rowtotal(PRI PVEM PT PANAL MORENA PES PAN_PRD_MC PT_MORENA CI_1)

foreach var in PRI PVEM PT PANAL MORENA PES PAN_PRD_MC PT_MORENA CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PRI i_PVEM i_PT i_PANAL i_MORENA i_PES i_PAN_PRD_MC i_PT_MORENA i_CI_1, gen(PRI_r PVEM_r PT_r PANAL_r MORENA_r PES_r PAN_PRD_MC_r PT_MORENA_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PVEM PT PANAL MORENA PES PAN_PRD_MC PT_MORENA CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

g year=2018
g month="July"
g STATE="TABASCO"

gen incumbent=""
gen incumbent_vote=.

foreach i of numlist 1 2 4 9 10 11 14 {
	local id = `i' + 27000
	replace incumbent="PRD_PANAL" if uniqueid==`id'
	replace incumbent_vote=PAN_PRD_MC if uniqueid==`id'
}

foreach i of numlist 6 8 {
	local id = `i' + 27000
	replace incumbent="PRD" if uniqueid==`id'
	replace incumbent_vote=PAN_PRD_MC if uniqueid==`id'
}

replace incumbent="PAN" if uniqueid==27013
replace incumbent_vote=PAN_PRD_MC if uniqueid==27013

replace incumbent="MORENA" if uniqueid==27005
replace incumbent_vote=MORENA if uniqueid==27005

foreach i of numlist 3 12 15 17 {
	local id = `i' + 27000
	replace incumbent="PRI" if uniqueid==`id'
	replace incumbent_vote=PRI if uniqueid==`id'
}

foreach i of numlist 7 16 {
	local id = `i' + 27000
	replace incumbent="PVEM" if uniqueid==`id'
	replace incumbent_vote=PVEM if uniqueid==`id'
} 

save Tabasco_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Tabasco_Section_2018.dta
collapse (sum) PRI-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Tabasco_2018.dta", replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

clear
append using Tabasco_Section_2015.dta
append using Tabasco_Section_2018.dta

erase Tabasco_Section_2015.dta
erase Tabasco_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
drop winner_counter

save Tabasco_Section_15_18.dta, replace

use "..\..\Precinct\Tabasco_ALL.dta", clear
append using Tabasco_Section_15_18.dta

replace municipality = "EMILIANO ZAPATA" if municipality=="E. ZAPATA"
replace municipality = "JALPA DE MENDEZ" if municipality=="J. DE MENDEZ"

save Tabasco_ALL_SALVADOR.dta, replace 

erase Tabasco_Section_15_18.dta
