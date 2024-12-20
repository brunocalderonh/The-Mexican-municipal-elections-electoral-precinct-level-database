////////////////////////////////////////////
////////NUEVO LEON PRECINCT PANEL UPDATE////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Nuevo Leon"

import excel "Ayuntamientos_2015.xlsx", sheet("Ayuntamientos_2015") clear firstrow 

gen coalprdpt=0
foreach i of numlist 1 9 10 16 18 25 33 37 44 47 49 50 51 {
	local id = `i' + 19000
	replace coalprdpt=1 if uniqueid==`id'
}

gen coalpripvemnapd=0 
foreach i of numlist 7 10 13 22 26 27 31 33 39 47 48 49 51 {
	local id = `i' + 19000
	replace coalpripvemnapd=1 if uniqueid==`id'
}

replace PRD_PT=PRD_PT + PRD + PT if coalprdpt==1
replace PRD=. if coalprdpt==1
replace PT=. if coalprdpt==1

replace PRI_PVEM_PANAL_PD = PRI_PVEM_PANAL_PD + PRI_PVEM_PANAL + PRI_PVEM_PD + PRI_PANAL_PD + PVEM_PANAL_PD + PRI_PVEM + PRI_PANAL + PRI_PD + PVEM_PANAL + PVEM_PD + PANAL_PD + PRI + PVEM + PANAL + PD if coalpripvemnapd==1
replace PD=. if coalpripvemnapd==1
replace PRI=. if coalpripvemnapd==1
replace PVEM=. if coalpripvemnapd==1
replace PANAL=. if coalpripvemnapd==1
drop PRI_PVEM_PANAL PRI_PVEM_PD PRI_PANAL_PD PVEM_PANAL_PD PRI_PVEM PRI_PANAL PRI_PD PVEM_PANAL PVEM_PD PANAL_PD

rename CI CI_1

collapse (sum) PAN- PRD_PT listanominal total, by(municipality section uniqueid)

egen valid=rowtotal(PAN PRI PRD PT PVEM MC PANAL PD PCC MORENA PH PES PRI_PVEM_PANAL_PD PRD_PT CI_1)

foreach var in PAN PRI PRD PT PVEM MC PANAL PD PCC MORENA PH PES PRI_PVEM_PANAL_PD PRD_PT CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_PD i_PCC i_MORENA i_PH i_PES i_PRI_PVEM_PANAL_PD i_PRD_PT i_CI_1, ///
	gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r PD_r PCC_r MORENA_r PH_r PES_r PRI_PVEM_PANAL_PD_r PRD_PT_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL PD PCC MORENA PH PES PRI_PVEM_PANAL_PD PRD_PT CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1" 

gen STATE="NUEVO LEON"
gen  year=2015
gen month="June"

replace municipality = upper(municipality)
drop uniqueid
gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANáHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMéNEZ"
replace uniqueid=19010 if municipality =="EL CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIéNEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZáLEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCíA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERáN"
replace uniqueid=19023 if municipality =="GRAL. TREVIñO"
replace uniqueid=19024 if municipality =="GRAL. ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUáREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARíN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARáS"
replace uniqueid=19041 if municipality =="PESQUERíA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLáS DE LOS GARZA"
replace uniqueid=19019 if municipality =="SAN PEDRO GARZA GARCíA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"

save Nuevo_Leon_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent

save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Nuevo_Leon_Section_2015.dta
collapse (sum) listanominal-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Nuevo_Leon_2015.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

import excel "Ayuntamientos_2018.xlsx", clear firstrow 

rename (MUNICIPIO SECCION VotosAnulados LISTA_NOMINAL) (municipality section nulo listanominal)

gen coalpripvem=0
foreach i in "Abasolo" "Los Aldamas" "Cadereyta Jimenez" "El Carmen" "Dr. Gonzalez" "Guadalupe" "Hidalgo" "Juarez" "Lampazos de Naranjo" "Mier y Noriega" "Los Ramones" "Salinas Victoria" "Villaldama" {
	replace coalpripvem=1 if municipality=="`i'"
}

gen coalpbt=1
foreach i in "Agualeguas" "Los Aldamas" "China" "Dr. Coss" "Melchor Ocampo" {
	replace coalpbt=0 if municipality=="`i'"
}

rename C_* *

replace PRI_PVEM = PRI_PVEM + PRI + PVEM if coalpripvem==1
replace PRI=. if coalpripvem==1
replace PVEM=. if coalpripvem==1

replace PT_MORENA_PES = PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES + MORENA + PT + PES if coalpbt==1
replace PT=. if coalpbt==1
replace PES=. if coalpbt==1
replace MORENA=. if coalpbt==1
drop PT_MORENA PT_PES MORENA_PES

rename NAPPN PANAL

egen total = rowtotal(PAN-CI_6)

collapse (sum) PAN-CI_6 total listanominal (first) coal*, by (uniqueid municipality section)

egen valid=rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PES RED PRI_PVEM PT_MORENA_PES CI_1 CI_2 CI_3 CI_4 CI_5 CI_6)

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES RED PRI_PVEM PT_MORENA_PES CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

gen turnout=total/listanominal
gen mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PES i_RED i_PRI_PVEM i_PT_MORENA_PES i_CI_1 i_CI_2 i_CI_3 i_CI_4 i_CI_5 i_CI_6, ///
	gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PES_r RED_r PRI_PVEM_r PT_MORENA_PES_r CI_1_r CI_2_r CI_3_r CI_4_r CI_5_r CI_6_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES RED PRI_PVEM PT_MORENA_PES CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

gen year=2018
gen month="July"
gen STATE="NUEVO LEON"

replace municipality = upper(municipality)
drop uniqueid
gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANAHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMENEZ"
replace uniqueid=19010 if municipality =="EL CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIENEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZALEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCIA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERAN"
replace uniqueid=19023 if municipality =="GRAL. TREVINO"
replace uniqueid=19024 if municipality =="GRAL. ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUAREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARIN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARAS"
replace uniqueid=19041 if municipality =="PESQUERIA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLAS DE LOS GARZA"
replace uniqueid=19019 if municipality =="SAN PEDRO GARZA GARCIA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta

gen incumbent_vote = .

foreach var in PAN PRD PRI PANAL MC PT {
	replace incumbent_vote=`var' if incumbent=="`var'"
}

replace incumbent_vote=PRI_PVEM if (incumbent=="PRI" | incumbent=="PRI_PVEM_PANAL_PD") & coalpripvem==1

replace incumbent_vote=PRI if incumbent=="PRI_PVEM_PANAL_PD" & coalpripvem==0

replace incumbent_vote=PRD if incumbent=="PRD_PT" 
replace incumbent_vote=PT if uniqueid==19016 & coalpbt==0

replace incumbent_vote=PT_MORENA_PES if incumbent=="PT" & coalpbt==1
replace incumbent_vote=PT_MORENA_PES if uniqueid==19016 & coalpbt==1

drop coal*

save Nuevo_Leon_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Nuevo_Leon_2018.dta", replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

///////////////////////////////////////////////
////////2018 MONTERRY EXTRAORDINARIO///////////
///////////////////////////////////////////////

import excel "NL_AYUN_2018_EXTRAORDINARIO.xlsx", sheet("NL_AYUN_2018_EXTRAORDINARIO") cellrange(A6:AD1601) firstrow clear

rename (SECCION TOTAL_VOTOS_CALCULADO LISTA_NOMINAL) (section total listanominal)

g municipality = "MONTERREY EXTRAORDINARIO"
g uniqueid = 19039

collapse (sum) PAN-CI_1 total listanominal, by (uniqueid municipality section)

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL RED CI_1)

foreach var in PAN PRI PRD PT PVEM MC PANAL RED CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

gen turnout = total/listanominal
gen mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_RED i_CI_1, gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r RED_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL RED CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

gen year=2018
gen month="December"
gen STATE="NUEVO LEON"

save Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta, replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

clear
append using Nuevo_Leon_Section_2015.dta
append using Nuevo_Leon_Section_2018.dta
append using Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta

erase Nuevo_Leon_Section_2015.dta
erase Nuevo_Leon_Section_2018.dta
erase Nuevo_Leon_Section_2018_EXTRAORDINARIO.dta

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

save Nuevo_Leon_Section_15_18.dta, replace

clear
use "..\..\Precinct\Nuevo_Leon_ALL.dta"
append using Nuevo_Leon_Section_15_18.dta

erase Nuevo_Leon_Section_15_18.dta

replace municipality = upper(municipality)

save Nuevo_Leon_ALL_SALVADOR.dta, replace 
