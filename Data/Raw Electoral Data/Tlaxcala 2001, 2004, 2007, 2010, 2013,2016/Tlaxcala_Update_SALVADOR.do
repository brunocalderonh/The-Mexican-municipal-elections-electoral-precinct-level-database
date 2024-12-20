////////////////////////////////////////////////
////////TLAXCALA PRECINCT PANEL UPDATE//////////
////////JUN 2019////////////////////////////////
////////SALVADOR ASCENCIO///////////////////////
////////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Tlaxcala"

import excel using "Ayuntamientos_Tlaxcala_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_Tlaxcala_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if section==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "Table 1.dta"
append using "Table 2.dta"
append using "Table 3.dta"
append using "Table 4.dta"
append using "Table 5.dta"
append using "Table 7.dta"
append using "Table 8.dta"
append using "Table 9.dta"
append using "Table 11.dta"
append using "Table 13.dta"
append using "Table 15.dta"
append using "Table 16.dta"
append using "Table 17.dta"
append using "Table 18.dta"
append using "Table 19.dta"
append using "Table 20.dta"
append using "Table 22.dta"
append using "Table 23.dta"
append using "Table 25.dta"
append using "Table 26.dta"
append using "Table 27.dta"
append using "Table 28.dta"
append using "Table 29.dta"
append using "Table 30.dta"
append using "Table 31.dta"
append using "Table 32.dta"
append using "Table 33.dta"
append using "Table 34.dta"
append using "Table 35.dta"
append using "Table 36.dta"
append using "Table 37.dta"
append using "Table 38.dta"
append using "Table 39.dta"
append using "Table 40.dta"
append using "Table 41.dta"
append using "Table 43.dta"
append using "Table 44.dta"
append using "Table 45.dta"
append using "Table 46.dta"
append using "Table 47.dta"
append using "Table 48.dta"
append using "Table 49.dta"
append using "Table 50.dta"
append using "Table 51.dta"
append using "Table 52.dta"
append using "Table 53.dta"
append using "Table 54.dta"
append using "Table 55.dta"
append using "Table 56.dta"
append using "Table 57.dta"
append using "Table 59.dta"
append using "Table 61.dta"
append using "Table 62.dta"
append using "Table 63.dta"
append using "Table 64.dta"
append using "Table 65.dta"
append using "Table 66.dta"
append using "Table 67.dta"
append using "Table 68.dta"
append using "Table 70.dta"

drop P Q R S T

replace municipality=upper(municipality)
destring *, replace

drop if strpos(municipality,"TOTAL")>0
drop if municipality==""

collapse (sum) PAN-PVEM_PS, by (municipality section)

preserve
import excel using "uniqueids16.xlsx", first clear
save uniqueids16.dta, replace
restore

merge m:1 municipality using uniqueids16.dta
drop _merge
erase uniqueids16.dta

drop municipality
rename municipio municipality
order municipality uniqueid section *

drop validos total

order CI_*, a(PVEM_PS)
order no_reg nulo, a(CI_4)

egen valid=rowtotal(PAN PRI PRD_PT PVEM MC PANAL PAC PS MORENA PES PRD PT PRI_PANAL PRI_PVEM_PANAL_PS PRI_PVEM PRI_PANAL_PS PRI_PS PRI_PT PRI_PVEM_PANAL PVEM_PS CI_1 CI_2 CI_3 CI_4)

gen total= valid + no_reg + nulo
drop no_reg nulo					

foreach var in PAN PRI PRD_PT PVEM MC PANAL PAC PS MORENA PES PRD PT PRI_PANAL PRI_PVEM_PANAL_PS PRI_PVEM PRI_PANAL_PS PRI_PS PRI_PT PRI_PVEM_PANAL PVEM_PS CI_1 CI_2 CI_3 CI_4 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD_PT i_PVEM i_MC i_PANAL i_PAC i_PS i_MORENA i_PES i_PRD i_PT i_PRI_PANAL i_PRI_PVEM_PANAL_PS i_PRI_PVEM i_PRI_PANAL_PS i_PRI_PS i_PRI_PT i_PRI_PVEM_PANAL i_PVEM_PS i_CI_1 i_CI_2 i_CI_3 i_CI_4, ///
	gen(PAN_r PRI_r PRD_PT_r PVEM_r MC_r PANAL_r PAC_r PS_r MORENA_r PES_r PRD_r PT_r PRI_PANAL_r PRI_PVEM_PANAL_PS_r PRI_PVEM_r PRI_PANAL_PS_r PRI_PS_r PRI_PT_r PRI_PVEM_PANAL_r PVEM_PS_r CI_1_r CI_2_r CI_3_r CI_4_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT PVEM MC PANAL PAC PS MORENA PES PRD PT PRI_PANAL PRI_PVEM_PANAL_PS PRI_PVEM PRI_PANAL_PS PRI_PS PRI_PT PRI_PVEM_PANAL PVEM_PS CI_1 CI_2 CI_3 CI_4 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"   | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2016
gen month="June"
gen STATE="TLAXCALA"

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC PAC PES PS Independent {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
drop winner_counter

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep entidad municipio seccion lista file year month
keep if entidad==29 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN16_TLAX.dta, replace
restore

merge 1:1 section using LN16_TLAX.dta
drop if _merge==2
drop _merge
erase LN16_TLAX.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

save Tlaxcala_Section_2016.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Tlaxcala_2016.dta", replace

**********************************************************************

clear 
append using "..\..\Precinct\Tlaxcala_ALL.dta"
append using Tlaxcala_Section_2016.dta

erase Tlaxcala_Section_2016.dta

replace municipality = "ATLTZAYANCA" if municipality=="ALTZAYANCA"
replace municipality = "SAN LUIS TEOLOCHOLCO" if municipality=="TEOLOCHOLCO"

save Tlaxcala_ALL_SALVADOR.dta, replace

erase "Table 1.dta"
erase "Table 2.dta"
erase "Table 3.dta"
erase "Table 4.dta"
erase "Table 5.dta"
erase "Table 7.dta"
erase "Table 8.dta"
erase "Table 9.dta"
erase "Table 11.dta"
erase "Table 13.dta"
erase "Table 15.dta"
erase "Table 16.dta"
erase "Table 17.dta"
erase "Table 18.dta"
erase "Table 19.dta"
erase "Table 20.dta"
erase "Table 22.dta"
erase "Table 23.dta"
erase "Table 25.dta"
erase "Table 26.dta"
erase "Table 27.dta"
erase "Table 28.dta"
erase "Table 29.dta"
erase "Table 30.dta"
erase "Table 31.dta"
erase "Table 32.dta"
erase "Table 33.dta"
erase "Table 34.dta"
erase "Table 35.dta"
erase "Table 36.dta"
erase "Table 37.dta"
erase "Table 38.dta"
erase "Table 39.dta"
erase "Table 40.dta"
erase "Table 41.dta"
erase "Table 43.dta"
erase "Table 44.dta"
erase "Table 45.dta"
erase "Table 46.dta"
erase "Table 47.dta"
erase "Table 48.dta"
erase "Table 49.dta"
erase "Table 50.dta"
erase "Table 51.dta"
erase "Table 52.dta"
erase "Table 53.dta"
erase "Table 54.dta"
erase "Table 55.dta"
erase "Table 56.dta"
erase "Table 57.dta"
erase "Table 59.dta"
erase "Table 61.dta"
erase "Table 62.dta"
erase "Table 63.dta"
erase "Table 64.dta"
erase "Table 65.dta"
erase "Table 66.dta"
erase "Table 67.dta"
erase "Table 68.dta"
erase "Table 70.dta"
