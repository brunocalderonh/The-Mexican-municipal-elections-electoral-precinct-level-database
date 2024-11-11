////////////////////////////////////////////
////////DURANGO PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Durango"

import excel using "Ayuntamientos Municipio_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos Municipio_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipio="`sheetname'"
	save "`sheetname'.dta", replace
}

clear 
append using "01. CANATLÁN.dta" 
append using "02. CANELAS.dta" 
append using "03. CONETO DE COMONFORT.dta" 
append using "04. CUENCAMÉ.dta" 
append using "05. DURANGO.dta" 
append using "06. GÓMEZ PALACIO.dta" 
append using "07. GRAL. SIMÓN BOLÍVAR.dta" 
append using "08. GUADALUPE VICTORIA.dta" 
append using "09. GUANACEVÍ.dta" 
append using "10. HIDALGO.dta" 
append using "11. INDÉ.dta" 
append using "12. LERDO.dta" 
append using "13. MAPIMÍ.dta" 
append using "14. MEZQUITAL.dta" 
append using "15. NAZAS.dta" 
append using "16. NOMBRE DE DIOS.dta" 
append using "17. NUEVO IDEAL.dta" 
append using "18. OCAMPO.dta" 
append using "19. EL ORO.dta" 
append using "20. OTAÉZ.dta" 
append using "21. PÁNUCO DE CORONADO.dta" 
append using "22. PEÑÓN BLANCO.dta" 
append using "23. POANAS.dta" 
append using "24. PUEBLO NUEVO.dta" 
append using "25. RODEO.dta" 
append using "26. SAN BERNARDO.dta" 
append using "27. SAN DIMAS.dta" 
append using "28. SAN JUAN DE GUADALUPE.dta" 
append using "29. SAN JUAN DEL RÍO.dta" 
append using "30. SAN LUIS DEL CORDERO.dta" 
append using "31. SAN PEDRO DEL GALLO.dta" 
append using "32. SANTA CLARA.dta" 
append using "33. SANTIAGO PAPASQUIARO.dta" 
append using "34. SÚCHIL.dta" 
append using "35. TAMAZULA.dta" 
append using "36. TEPEHUANES.dta" 
append using "37. TLAHUALILO.dta" 
append using "38. TOPIA.dta" 
append using "39. VICENTE GUERRERO.dta"

erase "01. CANATLÁN.dta" 
erase "02. CANELAS.dta" 
erase "03. CONETO DE COMONFORT.dta" 
erase "04. CUENCAMÉ.dta" 
erase "05. DURANGO.dta" 
erase "06. GÓMEZ PALACIO.dta" 
erase "07. GRAL. SIMÓN BOLÍVAR.dta" 
erase "08. GUADALUPE VICTORIA.dta" 
erase "09. GUANACEVÍ.dta" 
erase "10. HIDALGO.dta" 
erase "11. INDÉ.dta" 
erase "12. LERDO.dta" 
erase "13. MAPIMÍ.dta" 
erase "14. MEZQUITAL.dta" 
erase "15. NAZAS.dta" 
erase "16. NOMBRE DE DIOS.dta" 
erase "17. NUEVO IDEAL.dta" 
erase "18. OCAMPO.dta" 
erase "19. EL ORO.dta" 
erase "20. OTAÉZ.dta" 
erase "21. PÁNUCO DE CORONADO.dta" 
erase "22. PEÑÓN BLANCO.dta" 
erase "23. POANAS.dta" 
erase "24. PUEBLO NUEVO.dta" 
erase "25. RODEO.dta" 
erase "26. SAN BERNARDO.dta" 
erase "27. SAN DIMAS.dta" 
erase "28. SAN JUAN DE GUADALUPE.dta" 
erase "29. SAN JUAN DEL RÍO.dta" 
erase "30. SAN LUIS DEL CORDERO.dta" 
erase "31. SAN PEDRO DEL GALLO.dta" 
erase "32. SANTA CLARA.dta" 
erase "33. SANTIAGO PAPASQUIARO.dta" 
erase "34. SÚCHIL.dta" 
erase "35. TAMAZULA.dta" 
erase "36. TEPEHUANES.dta" 
erase "37. TLAHUALILO.dta" 
erase "38. TOPIA.dta" 
erase "39. VICENTE GUERRERO.dta" 

destring *, replace
drop if strpos(NOYDTTO, "DTTO")>0
drop L M N O P Q

gen munno=substr(municipio,1,2)
replace munno="06" if municipio=="07. GRAL. SIMÓN BOLÍVAR"
replace munno="07" if municipio=="06. GÓMEZ PALACIO"
destring munno, replace
replace munno=munno-1 if munno>=18
replace munno=39 if municipio=="17. NUEVO IDEAL"
gen uniqueid=munno+10000
gen municipality=substr(municipio,4,.)
drop municipio

rename SECCIÓN section
drop if section==.

rename PANPRD PAN_PRD
rename PRIPVEMPDPNA PRI_PVEM_PD_PANAL
rename TOTAL total
rename PNA PANAL
rename LNOMINAL listanominal

order PRI-PD, a(PES)
order CI_1 CI_2, a(PD)

collapse (sum) PAN_PRD-CI_2 listanominal total, by (municipality section uniqueid)

egen valid = rowtotal(PAN_PRD PRI_PVEM_PD_PANAL PT MORENA PES PRI PVEM PANAL MC PD CI_1 CI_2)

foreach var in PAN_PRD PRI_PVEM_PD_PANAL PT MORENA PES PRI PVEM PANAL MC PD CI_1 CI_2 total valid listanominal{
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN_PRD i_PRI_PVEM_PD_PANAL i_PT i_MORENA i_PES i_PRI i_PVEM i_PANAL i_MC i_PD i_CI_1 i_CI_2 , gen( PAN_PRD_r PRI_PVEM_PD_PANAL_r PT_r MORENA_r PES_r PRI_r PVEM_r PANAL_r MC_r PD_r  CI_1_r CI_2_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD PRI_PVEM_PD_PANAL PT MORENA PES PRI PVEM PANAL MC PD CI_1 CI_2 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"  
replace second="Independent" if second=="CI_1"  | second=="CI_2"
replace third="Independent" if third=="CI_1" | third=="CI_2" 

gen year=2016
gen month="June"
ge STATE="DURANGO"

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

save Durango_Section_2016.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN_PRD-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Durango_2016.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

import delimited "2019_SEE_AYUN _DGO_CAS.csv", clear

rename seccion section
rename municipio_local municipality
rename lista_nominal listanominal
rename total_votos total

g PAN_PRD = pan + prd + coal_pan_prd if coal_pan_prd!=.
replace pan = . if coal_pan_prd==.
replace prd = . if coal_pan_prd==.
drop coal_pan_prd
order PAN_PRD, a(morena)
rename (pan pri prd pvem pt mc pd morena cand_ind1 cand_ind2 cand_ind3 cand_ind4 cand_ind5 cand_ind6) (PAN PRI PRD PVEM PT MC PD MORENA CI_1 CI_2 CI_3 CI_4 CI_5 CI_6)

collapse (sum) PAN-CI_6 listanominal total, by (municipality section)

g uniqueid = .
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO" | municipality=="PEÃON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

order uniqueid, a(municipality)

egen valid= rowtotal(PAN PRI PRD PVEM PT MC PD MORENA PAN_PRD CI_1 CI_2 CI_3 CI_4 CI_5 CI_6)

foreach var in PAN PRI PRD PVEM PT MC PD MORENA PAN_PRD CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PD i_MORENA i_PAN_PRD i_CI_1 i_CI_2 i_CI_3 i_CI_4 i_CI_5 i_CI_6, gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PD_r MORENA_r PAN_PRD_r CI_1_r CI_2_r CI_3_r CI_4_r CI_5_r CI_6_r)
drop i_*

g winner = ""
g second = ""
g third = ""

foreach var in PAN PRI PRD PVEM PT MC PD MORENA PAN_PRD CI_1 CI_2 CI_3 CI_4 CI_5 CI_6 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

g year = 2019
g month = "June"
g STATE = "DURANGO"

g PAN_winner = 0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
g winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC PD {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Durango_Section_2019.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-valid (first) STATE year month winner second third mun_turnout , by(municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Durango_2019.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

use "..\..\Precinct\Durango_ALL.dta", clear
append using Durango_Section_2016.dta
append using Durango_Section_2019.dta

erase Durango_Section_2016.dta
erase Durango_Section_2019.dta

save Durango_all_SALVADOR.dta, replace
