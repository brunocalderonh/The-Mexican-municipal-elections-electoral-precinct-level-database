////////////////////////////////////////////
////////SONORA PRECINCT PANEL UPDATE////////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Sonora"

import excel "ComputoMpalAyuntamiento2015_casilla.xlsx", sheet("RES. CASILLA MUNICIPIOS") clear firstrow 

rename MUNICIPIO municipality
gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ÁLAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADÉHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BÁCUM"
replace uniqueid=26013 if municipality =="BANÁMICHI"
replace uniqueid=26014 if municipality =="BAVIÁCORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUÁREZ"
replace uniqueid=26016 if municipality =="BENJAMÍN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBÓ"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL. PLUTARCO ELÍAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUÁSABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUÉPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATÁN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NÁCORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCÍA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PEÑASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYÓN"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESÚS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RÍO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RÍO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SÁRIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YÉCORA"

rename municipality MUNICIPIO
order uniqueid, a(MUNICIPIO)

rename (MUNICIPIO SECCION NA PRIPVEMPNA PRIPVEM PRIPNA PVEMPNA NOREG NULOS PMOR PHUM) (municipality section PANAL PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL no_reg nulo MORENA PH)
	   
egen CI_1=rowtotal(JFC RFMM CMR CAVL)
drop JFC RFMM CMR CAVL

replace PRI_PVEM_PANAL=PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if PRI_PVEM_PANAL!=.
replace PRI=. if PRI_PVEM_PANAL!=.
replace PVEM=. if PRI_PVEM_PANAL!=.
replace PANAL=. if PRI_PVEM_PANAL!=.
drop PRI_PVEM PRI_PANAL PVEM_PANAL

collapse (sum) PAN-CI_1, by (municipality uniqueid section)

g year=2015
g month="June"
g STATE="SONORA"

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRI_PVEM_PANAL CI_1)

gen total = valid + nulo + no_reg
drop nulo no_reg

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRI_PVEM_PANAL CI_1 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PH i_PES i_PRI_PVEM_PANAL i_CI_1, gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PH_r PES_r PRI_PVEM_PANAL_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PH PES PRI_PVEM_PANAL CI_1{
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

preserve
import excel "ComputoMpalAyuntamiento2015_casilla.xlsx", sheet("incumbents") clear firstrow 
save incumbents2015.dta, replace
restore

merge m:1 municipality using incumbents2015.dta
drop _merge incumbent
erase incumbents2015.dta

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

order STATE municipality section year month *

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election (June 30 2015) 
keep entidad municipio seccion lista file year month
keep if entidad==26 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_SON.dta, replace
restore

merge 1:1 section using LN15_SON.dta
drop if _merge==2
drop _merge
erase LN15_SON.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout=total/listanominal

order STATE municipality uniqueid section year month *
sort uniqueid section

save Sonora_Section_2015.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Sonora_Section_2015.dta
collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Sonora_2015.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

import excel "Ayuntamientos_2018.xlsx", sheet("Sheet1") clear firstrow 

drop uniqueid
gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BACUM"
replace uniqueid=26013 if municipality =="BANAMICHI"
replace uniqueid=26014 if municipality =="BAVIACORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUAREZ"
replace uniqueid=26016 if municipality =="BENJAMIN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBO"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL. PLUTARCO ELIAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUASABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUEPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATAN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NACORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCIA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PEÑASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYON"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESUS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RIO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RIO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SARIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YECORA"

order uniqueid, a(municipality)

g PAN_PRD = CO_PAN_PRD + CO_PAN + CO_PRD 
replace PAN_PRD = CC_PAN_PRD if CC_PAN_PRD!=.
drop CO_PAN_PRD CO_PAN CO_PRD CC_PAN_PRD

g PRI_PVEM_PANAL = CO_PRI_PVEM_PANAL + CO_PRI_PVEM + CO_PRI_NA + CO_PVEM_NA + CO_PRI + CO_PVEM + CO_NA
replace PRI_PVEM_PANAL=CC_PRI_NA_PVEM if CC_PRI_NA_PVEM!=.
drop CO_PRI_PVEM CO_PRI_NA CO_PVEM_NA CO_PRI CO_PVEM CO_NA CO_PRI_PVEM_PANAL CC_PRI_NA_PVEM

g PT_MORENA_PES = CO_PT_MORENA_ES + CO_PT_MORENA + CO_PT_PES + CO_MORENA_PES + CO_PT + CO_MORENA + CO_PES
drop CO_PT_MORENA_ES CO_PT_MORENA CO_PT_PES CO_MORENA_PES CO_PT CO_MORENA CO_PES

rename CAND_* CI_*

egen ind1=rowtotal(CI_1-CI_13)
drop CI_*
rename ind1 CI_1

order PAN_PRD PRI_PVEM_PANAL PT_MORENA_PES CI_1, a(MAS)

rename LISTA_NOMINAL listanominal

collapse (sum) PAN-VOTOS_NULOS listanominal, by (municipality uniqueid section)

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PES MAS PAN_PRD PRI_PVEM_PANAL PT_MORENA_PES CI_1)

gen total= valid + CANDIDATO_NO_REGISTRADO + VOTOS_NULOS
drop CANDIDATO_NO_REGISTRADO VOTOS_NULOS

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES MAS PAN_PRD PRI_PVEM_PANAL PT_MORENA_PES CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PES i_MAS i_PT_MORENA_PES i_PAN_PRD i_PRI_PVEM_PANAL i_CI_1, ///
	gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PES_r MAS_r PT_MORENA_PES_r PAN_PRD_r PRI_PVEM_PANAL_r  CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES MAS PT_MORENA_PES PAN_PRD PRI_PVEM_PANAL CI_1{
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
g STATE="SONORA"

merge m:1 uniqueid using incumbents2018.dta
drop _merge

gen incumbent_vote = .

*Differentiate those with PRI=0 or PRI_PVEM_PANAL=0
egen maxpripvempri=rowmax(PRI_PVEM_PANAL PRI)
replace incumbent_vote=maxpripvempri if incumbent=="PRI" | incumbent=="PRI_PVEM_PANAL"

egen maxpripvempvem= rowmax(PRI_PVEM_PANAL PVEM)
replace incumbent_vote=maxpripvempvem if incumbent=="PVEM"

egen maxpanprdpan=rowmax(PAN_PRD PAN)
replace incumbent_vote=maxpanprdpan if incumbent=="PAN"

egen maxpanprdprd=rowmax(PAN_PRD PRD)
replace incumbent_vote=maxpanprdprd if incumbent=="PRD"

egen maxpripanal=rowmax(PRI_PVEM_PANAL PANAL) 
replace incumbent_vote=maxpripanal if incumbent=="PANAL"

replace incumbent_vote=MC if incumbent=="MC"

drop maxp*

order STATE municipality section year month *

save Sonora_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Sonora_Section_2018.dta
collapse (sum) PAN-total incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Sonora_2018.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

clear
append using Sonora_Section_2015.dta
append using Sonora_Section_2018.dta

erase Sonora_Section_2015.dta
erase Sonora_Section_2018.dta

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

save Sonora_Section_15_18.dta, replace

use "..\..\Precinct\Sonora_ALL.dta", clear
append using Sonora_Section_15_18.dta

replace municipality = upper(municipality)
replace municipality = subinstr(municipality, "GRAL.", "GENERAL", .)
replace municipality = subinstr(municipality, "GRAL .", "GENERAL", .)

save Sonora_ALL_SALVADOR.dta, replace 

erase Sonora_Section_15_18.dta
