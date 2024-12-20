////////////////////////////////////////////
////////TAMAULIPAS PRECINCT PANEL UPDATE////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Tamaulipas"

import excel using "Ayuntamientos_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipio="`sheetname'"
	drop if Casilla==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear 
append using "ABASOLO.dta"
append using "ALDAMA.dta"
append using "ALTAMIRA.dta"
append using "ANTIGUO MORELOS.dta"
append using "BURGOS.dta"
append using "BUSTAMANTE.dta"
append using "CAMARGO.dta"
append using "CASAS.dta"
append using "CIUDAD MADERO.dta"
append using "CRUILLAS.dta"
append using "GOMEZ FARIAS.dta"
append using "GONZALEZ.dta"
append using "GUEMEZ.dta"
append using "GUERRERO.dta"
append using "GUSTAVO DIAZ ORDAZ.dta"
append using "HIDALGO.dta"
append using "JAUMAVE.dta"
append using "JIMENEZ.dta"
append using "LLERA.dta"
append using "MAINERO.dta"
append using "EL MANTE.dta"
append using "MATAMOROS.dta"
append using "MENDEZ.dta"
append using "MIER.dta"
append using "MIGUEL ALEMAN.dta"
append using "MIQUIHUANA.dta"
append using "NUEVO LAREDO.dta"
append using "NUEVO MORELOS.dta"
append using "OCAMPO.dta"
append using "PADILLA.dta"
append using "PALMILLAS.dta"
append using "REYNOSA.dta"
append using "RIO BRAVO.dta"
append using "SAN CARLOS.dta"
append using "SAN FERNANDO.dta"
append using "SAN NICOLAS.dta"
append using "SOTO LA MARINA.dta"
append using "TAMPICO.dta"
append using "TULA.dta"
append using "VALLE HERMOSO.dta"
append using "VICTORIA.dta"
append using "VILLAGRAN.dta"
append using "XICOTENCATL.dta"

erase "ABASOLO.dta"
erase "ALDAMA.dta"
erase "ALTAMIRA.dta"
erase "ANTIGUO MORELOS.dta"
erase "BURGOS.dta"
erase "BUSTAMANTE.dta"
erase "CAMARGO.dta"
erase "CASAS.dta"
erase "CIUDAD MADERO.dta"
erase "CRUILLAS.dta"
erase "GOMEZ FARIAS.dta"
erase "GONZALEZ.dta"
erase "GUEMEZ.dta"
erase "GUERRERO.dta"
erase "GUSTAVO DIAZ ORDAZ.dta"
erase "HIDALGO.dta"
erase "JAUMAVE.dta"
erase "JIMENEZ.dta"
erase "LLERA.dta"
erase "MAINERO.dta"
erase "EL MANTE.dta"
erase "MATAMOROS.dta"
erase "MENDEZ.dta"
erase "MIER.dta"
erase "MIGUEL ALEMAN.dta"
erase "MIQUIHUANA.dta"
erase "NUEVO LAREDO.dta"
erase "NUEVO MORELOS.dta"
erase "OCAMPO.dta"
erase "PADILLA.dta"
erase "PALMILLAS.dta"
erase "REYNOSA.dta"
erase "RIO BRAVO.dta"
erase "SAN CARLOS.dta"
erase "SAN FERNANDO.dta"
erase "SAN NICOLAS.dta"
erase "SOTO LA MARINA.dta"
erase "TAMPICO.dta"
erase "TULA.dta"
erase "VALLE HERMOSO.dta"
erase "VICTORIA.dta"
erase "VILLAGRAN.dta"
erase "XICOTENCATL.dta"

destring *, replace

keep Casilla-PABLOTORRESLARA JOSEADAME XICOTENCATLGONZALEZURESTI
drop U

egen CI_1=rowtotal(JUANCUAUHTEMOCGARCIATAMEZ-JOSERAMONGOMEZLEAL CARLOSRAFAELULIVARRILOPEZ-XICOTENCATLGONZALEZURESTI)
egen CI_2=rowtotal(AMANDOTREVIÑORIVERA HECTORPEÑASALDAÑA JOSERAMONGOMEZLEAL PABLOTORRESLARA)
egen CI_3=rowtotal(JESUSROBERTOGUERRAVELASCO EDUARDOLONGORIACHAPA CARLOSRAFAELULIVARRILOPEZ)

replace CI_1=CI_1 - CI_2 - CI_3

drop JUANCUAUHTEMOCGARCIATAMEZ-JOSERAMONGOMEZLEAL CARLOSRAFAELULIVARRILOPEZ-XICOTENCATLGONZALEZURESTI 

gen section=substr(Casilla,1,4)
replace section=substr(section,1,3) if strpos(section,"B")>0
replace section=substr(section,1,3) if strpos(section,"C")>0
replace section=substr(section,1,3) if strpos(section,"E")>0
destring section, replace
drop if section==.

rename (municipio PNA Morena ES CoaliciónPRIPVEMyPANAL PRIPVEM PRIPNA PVEMPNA CandidatosNoRegistrados VotosNulos) (municipality PANAL MORENA PES PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL no_reg nulo)

replace PRI_PVEM_PANAL =	PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if PRI_PVEM_PANAL!=.
replace PRI=. if PRI_PVEM_PANAL!=.
replace PVEM=. if PRI_PVEM_PANAL!=.
replace PANAL=. if PRI_PVEM_PANAL!=. 
drop PRI_PVEM PRI_PANAL PVEM_PANAL

order CI_*, a(PRI_)

collapse (sum) PAN-nulo, by (municipality section)

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PES PRI_PVEM_PANAL CI_1 CI_2 CI_3)

g total = valid + nulo + no_reg
drop nulo no_reg

preserve
import excel "uniqueid16.xlsx", first clear
save uniqueid16.dta, replace
restore

merge m:1 municipality using uniqueid16.dta
drop _merge

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep if entidad==28 & month==5
rename seccion section
rename lista listanominal
save LN16_TAMPS.dta, replace
restore

merge 1:1 section using LN16_TAMPS.dta, keepusing(listanominal)
drop if _merge==2
drop _merge
erase LN16_TAMPS.dta

g turnout = total/listanominal

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PRI_PVEM_PANAL CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_PRI_PVEM_PANAL i_CI_1 i_CI_2 i_CI_3, gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r PRI_PVEM_PANAL_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PRI_PVEM_PANAL CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

g PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
g winner_counter = PAN_winner

foreach var in PRI PRD PVEM PT MC PANAL MORENA PES Independent {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0

g year=2016
g month="June"
g STATE="TAMAULIPAS"

save Tamaulipas_Section_2016.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Tamaulipas_2016.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

cd "Ayuntamientos 2018"
quietly foreach x in "Abasolo" "Aldama" "Altamira" "AntiguoMorelos" "Burgos" "Bustamente" "Camargo" "Casas" "CiudadMadero" "Cruillas" "ElMante" "GomezFarias" "Gonzalez" "Guemez" "Guerrero" "GustavoDiazOrdaz" ///
	"Hidalgo" "Jaumave" "Jimenez" "Llera" "Mainero" "Matamoros" "Mendez" "Mier" "MiguelAleman" "Miquihuana" "NuevoLaredo" "NuevoMorelos" "Ocampo" "Padilla" "Palmillas" "Reynosa" "RioBravo" ///
	"SanCarlos" "SanFernando" "SanNicolas" "SotolaMarina" "Tampico" "Tula" "ValleHermoso" "Victoria" "Villagran" "Xicotencatl" {
	import excel "`x'.xlsx", clear
	drop if _n<=5
	export excel using "updated_`x'.xlsx", replace
	import excel "updated_`x'.xlsx", firstrow clear
	tostring _all, replace
	drop if SECCION==""
	foreach y of varlist * {
		replace `y' = "0" if `y'==""
	}
	save "`x'.dta", replace
	erase "updated_`x'.xlsx"
}

clear
quietly foreach x in "Abasolo" "Aldama" "Altamira" "AntiguoMorelos" "Burgos" "Bustamente" "Camargo" "Casas" "CiudadMadero" "Cruillas" "ElMante" "GomezFarias" "Gonzalez" "Guemez" "Guerrero" "GustavoDiazOrdaz" ///
	"Hidalgo" "Jaumave" "Jimenez" "Llera" "Mainero" "Matamoros" "Mendez" "Mier" "MiguelAleman" "Miquihuana" "NuevoLaredo" "NuevoMorelos" "Ocampo" "Padilla" "Palmillas" "Reynosa" "RioBravo" ///
	"SanCarlos" "SanFernando" "SanNicolas" "SotolaMarina" "Tampico" "Tula" "ValleHermoso" "Victoria" "Villagran" "Xicotencatl" {
	append using "`x'.dta"
	erase "`x'.dta"
}

cd ".."

destring *, replace

rename SECCION section
rename MUNICIPIO municipality
rename LISTA_NOMINAL listanominal
rename TOTAL total
rename NUM_VOTOS_VALIDOS valid

rename (JESUSOLVERAMENDEZ DAVIDPERALESSEGURA BEATRIZREYESNAJERA JOSELUISGALLARDOFLORES HECTORMANUELDELATORREVALENZ HUMBERTORANGELVALLEJO VICTORMANUELVERGARAMARTINEZ ///
	JORGELUISMIRANDANIÑO HECTORMICHELSALINASGAMEZ CARLOSALBERTOGUERREROGARCIA MIGUELANGELALMARAZMALDONADO CLAUDIOALBERTOCAPETILLOGOMEZ NAYMAKARINABALQUIARENAPEREZ HECTORDAVIDRUIZTAMAYO) ///
	(CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 CI_7 CI_8 CI_9 CI_10 CI_11 CI_12 CI_13 CI_14)

g MORENA_PT_PES = MORENA + PT + ES + PTMORENAES + PTMORENA + PTES + MORENAES if municipality!="BURGOS" & municipality!="NUVEO MORELOS"
quietly foreach x of varlist MORENA PT ES PTMORENAES PTMORENA PTES MORENAES {
	replace `x' = . if municipality!="BURGOS" & municipality!="NUVEO MORELOS"
}

g PAN_PRD_MC = PAN + PRD + MC + PANPRDMC + PANPRD + PANMC + PRDMC if municipality!="BURGOS" & municipality!="BUSTAMANTE" & municipality!="CASAS" & municipality!="CRUILLAS" & municipality!="EL MANTE" & ///
	 municipality!="GOMEZ FARIAS" & municipality!="GONZALEZ" & municipality!="GUERRERO" & municipality!="JAUMAVE" & municipality!="JIMENEZ" & municipality!="MENDEZ" & municipality!="MIER" & municipality!="MIQUIHUANA" & ///
	 municipality!="NUVEO MORELOS" & municipality!="OCAMPO" & municipality!="PADILLA" & municipality!="PALMILLAS" & municipality!="SAN NICOLAS" & municipality!="TULA" & municipality!="VILLAGRAN"
quietly foreach x of varlist PAN PRD MC PANPRDMC PANPRD PANMC PRDMC {
	replace `x' = . if municipality!="BURGOS" & municipality!="BUSTAMANTE" & municipality!="CASAS" & municipality!="CRUILLAS" & municipality!="EL MANTE" & ///
	 municipality!="GOMEZ FARIAS" & municipality!="GONZALEZ" & municipality!="GUERRERO" & municipality!="JAUMAVE" & municipality!="JIMENEZ" & municipality!="MENDEZ" & municipality!="MIER" & municipality!="MIQUIHUANA" & ///
	 municipality!="NUVEO MORELOS" & municipality!="OCAMPO" & municipality!="PADILLA" & municipality!="PALMILLAS" & municipality!="SAN NICOLAS" & municipality!="TULA" & municipality!="VILLAGRAN"
}

drop PTMORENAES PTMORENA PTES MORENAES PANPRDMC PANPRD PANMC PRDMC
rename NA PANAL
rename ES PES

collapse (sum) PAN PRI PRD PVEM PT MC PANAL MORENA PES MORENA_PT_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 CI_7 CI_8 CI_9 CI_10 CI_11 CI_12 CI_13 CI_14 valid total listanominal, by(municipality section)

replace municipality = "GUEMEZ" if municipality=="GÜEMEZ"
merge m:1 municipality using uniqueid16.dta
drop _merge
erase uniqueid16.dta

quietly foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES MORENA_PT_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 CI_7 CI_8 CI_9 CI_10 CI_11 CI_12 CI_13 CI_14 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_MORENA_PT_PES i_PAN_PRD_MC i_CI_1 i_CI_2 i_CI_3 i_CI_4 i_CI_5 i_CI_6 i_CI_7 i_CI_8 i_CI_9 i_CI_10 i_CI_11 i_CI_12 i_CI_13 i_CI_14, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r MORENA_PT_PES_r PAN_PRD_MC_r CI_1_r CI_2_r CI_3_r CI_4_r CI_5_r CI_6_r CI_7_r CI_8_r CI_9_r CI_10_r CI_11_r CI_12_r CI_13_r CI_14_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES MORENA_PT_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 CI_7 CI_8 CI_9 CI_10 CI_11 CI_12 CI_13 CI_14 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC Independent {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0

g year=2018
g month="July"
g STATE="TAMAULIPAS"

save Tamaulipas_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Tamaulipas_2018.dta", replace

**********************************************************************
**********************************************************************
**********************************************************************

use "..\..\Precinct\Tamaulipas_ALL.dta", clear
append using Tamaulipas_Section_2016.dta
append using Tamaulipas_Section_2018.dta

erase Tamaulipas_Section_2018.dta
erase Tamaulipas_Section_2016.dta

replace municipality = upper(municipality)
replace municipality = "PALMILLAS" if municipality=="PALMILLA"
replace municipality = "MIGUEL ALEMAN" if municipality=="MIGUEL A."
replace municipality = "ANTIGUO MORELOS" if municipality=="ANT. MORELOS" | municipality=="A. MORELOS"
replace municipality = "CIUDAD MADERO" if municipality=="CD. MADERO" | municipality=="CD.MADERO"
replace municipality = "GUSTAVO DIAZ ORDAZ" if strpos(municipality, "ORDAZ")>0 | municipality=="GVO. DIAZ O."
replace municipality = "EL MANTE" if strpos(municipality, "MANTE")>0 & municipality!="BUSTAMANTE"
replace municipality = "NUEVO MORELOS" if strpos(municipality, "MORELOS")>0 & municipality!="ANTIGUO MORELOS"
replace municipality = "NUEVO LAREDO" if strpos(municipality, "LAREDO")>0

save Tamaulipas_ALL_SALVADOR.dta, replace
