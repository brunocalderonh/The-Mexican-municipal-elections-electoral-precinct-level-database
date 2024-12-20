////////////////////////////////////////////
////////QUINTANA ROO PRECINCT PANEL UPDATE/
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Quintana Roo"

///////////
////2016///
///////////

import excel using "Ayuntamientos_QRoo_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_QRoo_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if SECCION==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "Table 2.dta"
append using "Table 4.dta"
append using "Table 6.dta"
append using "Table 8.dta"
append using "Table 10.dta"
append using "Table 12.dta"
append using "Table 14.dta"
append using "Table 16.dta"
append using "Table 18.dta"
append using "Table 20.dta"
append using "Table 22.dta"
append using "Table 24.dta"
append using "Table 26.dta"
append using "Table 28.dta"
append using "Table 30.dta"
append using "Table 32.dta"
append using "Table 34.dta"
append using "Table 36.dta"
append using "Table 38.dta"
append using "Table 40.dta"
append using "Table 42.dta"
append using "Table 44.dta"
append using "Table 48.dta"
append using "Table 50.dta"
append using "Table 52.dta"
append using "Table 56.dta"
append using "Table 58.dta"
append using "Table 64.dta"
append using "Table 68.dta"
append using "Table 74.dta"
append using "Table 80.dta"
append using "Table 82.dta"
append using "Table 84.dta"
append using "Table 86.dta"
append using "Table 88.dta"
append using "Table 90.dta"
append using "Table 92.dta"
append using "Table 98.dta"
append using "Table 100.dta"
append using "Table 102.dta"
append using "Table 104.dta"
append using "Table 106.dta"
append using "Table 108.dta"
append using "Table 110.dta"
append using "Table 114.dta"
append using "Table 116.dta"
append using "Table 120.dta"
append using "Table 126.dta"

destring *, replace

rename (SECCION CNR VN VT ES NA CI) (section no_reg nulo total PES PANAL CI_1)

replace PRIPVEMNA = PRIPVEMNA + PRIPVEM + PRINA + PVEMNA + PRI + PVEM + PANAL
drop PRIPVEM PRINA PVEMNA PRI PVEM PANAL
rename PRIPVEMNA PRI_PVEM_PANAL

replace PANPRD=PANPRD+PAN+PRD
drop PAN PRD
rename PANPRD PAN_PRD

collapse (sum) PT-CI_1 total, by (municipality uniqueid section)

egen valid = rowtotal(PT MC MORENA PES PAN_PRD PRI_PVEM_PANAL CI_1)

foreach var in PT MC MORENA PES PAN_PRD PRI_PVEM_PANAL CI_1 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN_PRD i_PRI_PVEM_PANAL i_PT i_MC i_MORENA i_PES i_CI_1, gen(PAN_PRD_r PRI_PVEM_PANAL_r PT_r MC_r MORENA_r PES_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PT MC MORENA PES PAN_PRD PRI_PVEM_PANAL CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

gen year=2016
gen month="June"
gen STATE="QUINTANA ROO"

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep entidad municipio seccion lista file year month
keep if entidad==23 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN16_QROO.dta, replace
restore

merge 1:1 section using LN16_QROO.dta
drop if _merge==2
drop _merge
erase LN16_QROO.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

save Quintana_Roo_Section_2016.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PT-valid listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Quintana_Roo_2016.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

///////////
////2018///
///////////

clear
import excel using "Ayuntamientos_QRoo_2018.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_QRoo_2018.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipality="`sheetname'"
	drop if section==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "BENITO JUAREZ.dta"
append using "COZUMEL.dta"
append using "FELIPE CARRILLO PUERTO.dta"
append using "ISLA MUJERES.dta"
append using "JOSE MARIA MORELOS.dta"
append using "LAZARO CARDENAS.dta"
append using "OTHON P. BLANCO.dta"
append using "SOLIDARIDAD.dta"
append using "TULUM.dta"
append using "BACALAR.dta"
append using "PUERTO MORELOS.dta"

destring *, replace

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if PRI_PVEM_PANAL!=.
replace PRI=. if PRI_PVEM_PANAL!=.
replace PVEM=. if PRI_PVEM_PANAL!=.
replace PANAL=. if PRI_PVEM_PANAL!=.
drop PRI_PVEM PRI_PANAL PVEM_PANAL PVEM

replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if PAN_PRD_MC!=.
drop PAN_PRD PAN_MC PRD_MC PAN PRD MC

replace PT_MORENA = PT_MORENA + PT + MORENA if PT_MORENA!=.
replace PT=. if PT_MORENA!=.
replace MORENA=. if PT_MORENA!=.

egen valid =rowtotal(PRI PT PANAL MORENA PES PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA CI_1)

foreach var in PRI PT PANAL MORENA PES PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA CI_1 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PRI i_PT i_PANAL i_MORENA i_PES i_PAN_PRD_MC i_PRI_PVEM_PANAL i_PT_MORENA i_CI_1, gen(PRI_r PT_r PANAL_r MORENA_r PES_r PAN_PRD_MC_r PRI_PVEM_PANAL_r PT_MORENA_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PT PANAL MORENA PES PAN_PRD_MC PRI_PVEM_PANAL PT_MORENA CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

drop no_reg nulo SEC

gen year=2018
gen month="July"
gen STATE="QUINTANA ROO"

preserve
use "..\Listas Nominales\ListadoNominalPREP2018.dta", clear
keep if STATE=="QUINTANA ROO"
save LN18_QROO.dta, replace
restore

merge 1:1 STATE section using LN18_QROO.dta
drop _merge 
erase LN18_QROO.dta

rename ListadoNominalINE listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g turnout = total/listanominal
g mun_turnout =  mun_total/mun_listanominal

save Quintana_Roo_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PRI-total valid listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Quintana_Roo_2018.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

clear
append using Quintana_Roo_Section_2016.dta
append using Quintana_Roo_Section_2018.dta

erase Quintana_Roo_Section_2016.dta
erase Quintana_Roo_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 & (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0

save Quintana_Roo_Section_16_18.dta, replace

use "..\..\Precinct\Quintana_Roo_ALL.dta", clear
append using Quintana_Roo_Section_16_18.dta

erase Quintana_Roo_Section_16_18.dta

replace municipality = upper(municipality)

save Quintana_Roo_ALL_SALVADOR.dta, replace 

*********************************************************************************************************************************************

erase "Table 2.dta"
erase "Table 4.dta"
erase "Table 6.dta"
erase "Table 8.dta"
erase "Table 10.dta"
erase "Table 12.dta"
erase "Table 14.dta"
erase "Table 16.dta"
erase "Table 18.dta"
erase "Table 20.dta"
erase "Table 22.dta"
erase "Table 24.dta"
erase "Table 26.dta"
erase "Table 28.dta"
erase "Table 30.dta"
erase "Table 32.dta"
erase "Table 34.dta"
erase "Table 36.dta"
erase "Table 38.dta"
erase "Table 40.dta"
erase "Table 42.dta"
erase "Table 44.dta"
erase "Table 48.dta"
erase "Table 50.dta"
erase "Table 52.dta"
erase "Table 56.dta"
erase "Table 58.dta"
erase "Table 64.dta"
erase "Table 68.dta"
erase "Table 74.dta"
erase "Table 80.dta"
erase "Table 82.dta"
erase "Table 84.dta"
erase "Table 86.dta"
erase "Table 88.dta"
erase "Table 90.dta"
erase "Table 92.dta"
erase "Table 98.dta"
erase "Table 100.dta"
erase "Table 102.dta"
erase "Table 104.dta"
erase "Table 106.dta"
erase "Table 108.dta"
erase "Table 110.dta"
erase "Table 114.dta"
erase "Table 116.dta"
erase "Table 120.dta"
erase "Table 126.dta"
erase "BENITO JUAREZ.dta"
erase "COZUMEL.dta"
erase "FELIPE CARRILLO PUERTO.dta"
erase "ISLA MUJERES.dta"
erase "JOSE MARIA MORELOS.dta"
erase "LAZARO CARDENAS.dta"
erase "OTHON P. BLANCO.dta"
erase "SOLIDARIDAD.dta"
erase "TULUM.dta"
erase "BACALAR.dta"
erase "PUERTO MORELOS.dta"
