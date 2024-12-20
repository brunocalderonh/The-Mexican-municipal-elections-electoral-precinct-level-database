////////////////////////////////////////////
////////SINALOA PRECINCT PANEL UPDATE////////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Sinaloa"

*See auxiliary file for Michoacan for the process to unify xlsx files
import excel using "Ayuntamientos_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipality="`sheetname'"
	drop if CASILLASECCIÓN==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "CHOIX.dta"
append using "FUERTE.dta"
append using "AHOME.dta"
append using "SINALOA.dta"
append using "GUASAVE.dta"
append using "ANGOSTURA.dta"
append using "SALVADOR ALVARADO.dta"
append using "MOCORITO.dta"
append using "BADIRAGUATO.dta"
append using "CULIACAN.dta"
append using "NAVOLATO.dta"
append using "COSALA.dta"
append using "ELOTA.dta"
append using "SAN IGNACIO.dta"
append using "MAZATLAN.dta"
append using "CONCORDIA.dta"
append using "ROSARIO.dta"
append using "ESCUINAPA.dta"

destring *, replace 
drop if CASILLASECCIÓN==.
drop O-N
order PVEM-CI_3, a(MC_MAS)

rename (CASILLASECCIÓN VOTOSACANDIDATOSNOREGISTRADO VOTOSNULOS VOTACIÓNTOTAL) (section no_reg nulo total)

gen uniqueid=.
replace uniqueid=25007 if municipality=="CHOIX"
replace uniqueid=25010 if municipality=="FUERTE"
replace uniqueid=25001 if municipality=="AHOME"
replace uniqueid=25017 if municipality=="SINALOA"
replace uniqueid=25011 if municipality=="GUASAVE"
replace uniqueid=25002 if municipality=="ANGOSTURA"
replace uniqueid=25015 if municipality=="SALVADOR ALVARADO"
replace uniqueid=25013 if municipality=="MOCORITO"
replace uniqueid=25003 if municipality=="BADIRAGUATO"
replace uniqueid=25006 if municipality=="CULIACAN"
replace uniqueid=25018 if municipality=="NAVOLATO"
replace uniqueid=25005 if municipality=="COSALA"
replace uniqueid=25008 if municipality=="ELOTA"
replace uniqueid=25016 if municipality=="SAN IGNACIO"
replace uniqueid=25012 if municipality=="MAZATLAN"
replace uniqueid=25004 if municipality=="CONCORDIA"
replace uniqueid=25014 if municipality=="ROSARIO"
replace uniqueid=25009 if municipality=="ESCUINAPA"

replace municipality="EL FUERTE" if  municipality=="FUERTE"

rename MC_MAS MC_PAS
rename MAS PAS

replace PRI_PANAL=PRI+PANAL+PRI_PANAL if PRI_PANAL!=.
replace PRI=. if PRI_PANAL!=.
replace PANAL=. if PRI_PANAL!=.

replace MC_PAS=MC+PAS+MC_PAS if MC_PAS!=.
replace MC=. if MC_PAS!=.
replace PAS=. if MC_PAS!=.

order municipality uniqueid section *

collapse (sum) PAN-CI_3 total, by (municipality uniqueid section)

egen valid = rowtotal(PAN PRI PRD PT MC PANAL PAS MORENA PRI_PANAL MC_PAS PVEM PES CI_1 CI_2 CI_3)

foreach var in PAN PRI PRD PT MC PANAL PAS MORENA PRI_PANAL MC_PAS PVEM PES CI_1 CI_2 CI_3 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PT i_MC i_PANAL i_PAS i_MORENA i_PRI_PANAL i_MC_PAS i_PVEM i_PES i_CI_1 i_CI_2 i_CI_3, ///
	gen(PAN_r PRI_r PRD_r PT_r MC_r PANAL_r PAS_r MORENA_r PRI_PANAL_r MC_PAS_r PVEM_r PES_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""
foreach var in PAN PRI PRD PT MC PANAL PAS MORENA PRI_PANAL MC_PAS PVEM PES CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" |  winner=="CI_2" | winner=="CI_3" 
replace second="Independent" if second=="CI_1"  | second=="CI_2"  | second=="CI_3" 
replace third="Independent" if third=="CI_1" | third=="CI_2" | third=="CI_3" 

g year=2016
g month="June"
g STATE="SINALOA"

g PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 & (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
g winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC PAS {
	gen `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0

drop if section==.

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep entidad municipio seccion lista file year month
keep if entidad==25 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN16_SIN.dta, replace
restore

merge 1:1 section using LN16_SIN.dta
drop if _merge==2
drop _merge
erase "LN16_SIN.dta"

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

save Sinaloa_Section_2016.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Sinaloa_Section_2016.dta
collapse (sum) PAN-valid listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Sinaloa_2016.dta", replace

**********************************************************************
**********************************************************************

erase "CHOIX.dta"
erase "FUERTE.dta"
erase "AHOME.dta"
erase "SINALOA.dta"
erase "GUASAVE.dta"
erase "ANGOSTURA.dta"
erase "SALVADOR ALVARADO.dta"
erase "MOCORITO.dta"
erase "BADIRAGUATO.dta"
erase "CULIACAN.dta"
erase "NAVOLATO.dta"
erase "COSALA.dta"
erase "ELOTA.dta"
erase "SAN IGNACIO.dta"
erase "MAZATLAN.dta"
erase "CONCORDIA.dta"
erase "ROSARIO.dta"
erase "ESCUINAPA.dta"

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

import excel "02 Resultados_Ayuntamientos_Sinaloa_2018_Casilla.xlsx", sheet("2018_SEE_AYUN_SIN_CAS") firstrow clear

rename SECCION section
drop if ESTATUS=="Paquete no entregado"
rename LISTA listanominal
rename TOTAL total
g municipality = upper(MUNICIPIO)

g PAN_PRD_MC_PAS = PAN + PRD + MC + PAS + C_PAN_PRD_MC_PAS + C_PAN_PRD_MC + C_PAN_PRD_PAS + C_PAN_MC_PAS + C_PRD_MC_PAS + C_PAN_PRD + C_PAN_MC + C_PAN_PAS + C_PRD_MC + C_PRD_PAS + C_MC_PAS
drop PAN PRD MC PAS C_PAN_PRD_MC_PAS C_PAN_PRD_MC C_PAN_PRD_PAS C_PAN_MC_PAS C_PRD_MC_PAS C_PAN_PRD C_PAN_MC C_PAN_PAS C_PRD_MC C_PRD_PAS C_MC_PAS

g PT_MORENA_PES = PT + MORENA + ES + C_PT_MORENA_ES + C_PT_MORENA + C_PT_ES + C_MORENA_ES if municipality!="CONCORDIA" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
replace PT = . if municipality!="CONCORDIA" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
replace MORENA = . if municipality!="CONCORDIA" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
rename ES PES
replace PES = . if municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
drop PT MORENA ES C_PT_MORENA_ES C_PT_MORENA C_PT_ES C_MORENA_ES

g PRI_PVEM_PANAL = PRI + PVEM + NA + CC_PRI_PVEM_NA + CC_PRI_PVEM + CC_PRI_NA + CC_PVEM_NA if municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
replace PRI = . if municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
replace PVEM = . if municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
rename NA PANAL
replace PANAL = . if municipality!="SAN IGNACIO" & municipality!="ROSARIO" & municipality!="ESCUINAPA"
drop CC_PRI_PVEM_NA CC_PRI_PVEM CC_PRI_NA CC_PVEM_NA

rename CAND_IND1 CI_1
rename CAND_IND2 CI_2
rename CAND_IND3 CI_3 
rename CAND_IND4 CI_4

order PAN_PRD_MC_PAS PT_MORENA_PES PRI_PVEM_PANAL, b(CI_1)
	
gen uniqueid=.
replace uniqueid=25007 if municipality=="CHOIX"
replace uniqueid=25010 if municipality=="EL FUERTE"
replace uniqueid=25001 if municipality=="AHOME"
replace uniqueid=25017 if municipality=="SINALOA"
replace uniqueid=25011 if municipality=="GUASAVE"
replace uniqueid=25002 if municipality=="ANGOSTURA"
replace uniqueid=25015 if municipality=="SALVADOR ALVARADO"
replace uniqueid=25013 if municipality=="MOCORITO"
replace uniqueid=25003 if municipality=="BADIRAGUATO"
replace uniqueid=25006 if municipality=="CULIACAN"
replace uniqueid=25018 if municipality=="NAVOLATO"
replace uniqueid=25005 if municipality=="COSALA"
replace uniqueid=25008 if municipality=="ELOTA"
replace uniqueid=25016 if municipality=="SAN IGNACIO"
replace uniqueid=25012 if municipality=="MAZATLAN"
replace uniqueid=25004 if municipality=="CONCORDIA"
replace uniqueid=25014 if municipality=="ROSARIO"
replace uniqueid=25009 if municipality=="ESCUINAPA"

collapse (sum) PRI-CI_4 total listanominal, by (municipality uniqueid section)

egen valid = rowtotal(PRI PVEM PANAL PES PAIS PAN_PRD_MC_PAS PT_MORENA_PES PRI_PVEM_PANAL CI_1 CI_2 CI_3 CI_4)

g turnout = total/listanominal

foreach var in PRI PVEM PANAL PES PAIS PAN_PRD_MC_PAS PT_MORENA_PES PRI_PVEM_PANAL CI_1 CI_2 CI_3 CI_4 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks i_PRI i_PVEM i_PANAL i_PES i_PAIS i_PAN_PRD_MC_PAS i_PT_MORENA_PES i_PRI_PVEM_PANAL i_CI_1 i_CI_2 i_CI_3 i_CI_4, gen(PRI_r PVEM_r PANAL_r PES_r PAIS_r PAN_PRD_MC_PAS_r PT_MORENA_PES_r PRI_PVEM_PANAL_r CI_1_r CI_2_r CI_3_r CI_4_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""
foreach var in PRI PVEM PANAL PES PAIS PAN_PRD_MC_PAS PT_MORENA_PES PRI_PVEM_PANAL CI_1 CI_2 CI_3 CI_4 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

g year=2018
g month="July"
g STATE="SINALOA"

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 & (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
	gen `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Sinaloa_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Sinaloa_Section_2018.dta
collapse (sum) PAN_PRD_MC_PAS-listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Sinaloa_2018.dta", replace

**********************************************************************
**********************************************************************
**********************************************************************

use "..\..\Precinct\Sinaloa_ALL.dta", clear
append using Sinaloa_Section_2016.dta
append using Sinaloa_Section_2018.dta

replace municipality = "ROSARIO" if municipality=="EL ROSARIO"
replace municipality = "MAZATLAN" if uniqueid==25012

save Sinaloa_ALL_SALVADOR.dta, replace 

erase Sinaloa_Section_2016.dta
erase Sinaloa_Section_2018.dta
