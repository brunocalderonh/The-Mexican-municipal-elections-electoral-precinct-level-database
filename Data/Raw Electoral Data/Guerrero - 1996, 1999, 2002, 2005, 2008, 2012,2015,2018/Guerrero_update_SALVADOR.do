////////////////////////////////////////////
////////GUERRERO PRECINCT PANEL UPDATE//////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Guerrero"

//////////////////////////////////
////////////2015//////////////////
//////////////////////////////////

import excel "Ayuntamientos_Gro_2015.xlsx", sheet("Ayuntamientos") clear firstrow allstring

destring *, replace

egen PRI_PVEM2 = rowtotal(PRI_PVEM PRI PVEM) if coalpripvem==1
replace PRI=. if coalpripvem==1
replace PVEM=. if coalpripvem==1

egen PRD_PT2 = rowtotal(PRD_PT PRD PT) if coalprdpt==1
replace PRD=. if coalprdpt==1
replace PT=. if coalprdpt==1

egen PRI_PVEM_PANAL2 = rowtotal(PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL PRI PVEM PANAL) if coalpripvempanal==1
replace PRI=. if coalpripvempanal==1
replace PVEM=. if coalpripvempanal==1 
replace PANAL=. if coalpripvempanal==1 

egen PRD_PT_PAN2 = rowtotal(PRD_PT_PAN PRD_PAN PT_PAN PRD_PT PRD PT PAN) if coalprdptpan==1
replace PRD=. if coalprdptpan==1
replace PT=. if coalprdptpan==1 
replace PAN=. if coalprdptpan==1 

drop PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL PRD_PT PRD_PT_PAN PRD_PAN PT_PAN
rename PRI_PVEM_PANAL2 PRI_PVEM_PANAL
rename PRI_PVEM2 PRI_PVEM
rename PRD_PT2 PRD_PT
rename PRD_PT_PAN2 PRD_PT_PAN

order PRI_PVEM_PANAL PRI_PVEM PRI_PVEM_PANAL PRD_PT, a(PPG)

drop uniqueid
gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12006 if municipality =="APAXTLA"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MARIA IZAZAGA"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MA IZAZAGA"
replace uniqueid=12078 if municipality =="COCHOAPA EL GRANDE"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GENERAL HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12081 if municipality =="ILIATENCO"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12079 if municipality =="JOSE JOAQUIN DE HERRERA"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12080 if municipality =="JUCHITAN"
replace uniqueid=12068 if municipality =="LA UNION DE ISIDORO MONTES DE OCA"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12077 if municipality =="MARQUELIA"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if municipality =="PETATLAN*" | strpos(municipality, "PETATLAN")>0
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="ZIHUATANEJO DE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO"
replace uniqueid=12074 if municipality =="ZITLALA"

drop if uniqueid==0

collapse (sum) PAN-CI_1 total listanominal, by (section municipality uniqueid)

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PPG PRI_PVEM_PANAL PRI_PVEM PRD_PT CI_1)

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PPG PRI_PVEM_PANAL PRI_PVEM PRD_PT CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PH i_PES i_PPG i_PRI_PVEM i_PRD_PT i_PRI_PVEM_PANAL i_CI_1, ///
	gen( PAN_r  PRI_r  PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PH_r PES_r PPG_r PRI_PVEM_r  PRD_PT_r PRI_PVEM_PANAL_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PPG PRI_PVEM_PANAL PRI_PVEM PRD_PT CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1" 

g year=2015
g month="June"
replace month = "November" if municipality=="TIXTLA DE GUERRERO"
replace municipality = "TIXTLA DE GUERRERO EXTRAORDINARIO" if municipality=="TIXTLA DE GUERRERO"
g STATE="GUERRERO"

save Guerrero_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
replace municipality = "TIXTLA DE GUERRERO" if municipality=="TIXTLA DE GUERRERO EXTRAORDINARIO"
rename winner incumbent

save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Guerrero_Section_2015.dta
collapse (sum) PAN-total (first) STATE year month winner second third mun_turnout, by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Guerrero_2015.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2018//////////////////
//////////////////////////////////

import excel "communal candidates 2018.xlsx", sheet("MUNCAND") firstrow clear

replace PAN_PRD = AH if PAN_PRD==.
replace PRD_MC = AI if PRD_MC==.
keep MUNICIPIO PAN_PRD_MC-MORENA_ES

foreach x of varlist PAN_PRD_MC-MORENA_ES {
	replace `x' = 0 if `x'==.
	replace `x' = 1 if `x'>0
}

rename * coal_*
rename coal_MUNICIPIO municipality

save coalindicators.dta, replace

import delimited "2018_SEE_GRO_CAS_AYUN.csv", case(preserve) clear 

replace PAN_PRD = v39 if PAN_PRD==.
replace PRD_MC = v40 if PRD_MC==.
drop v39 v40

rename (CAND_IND1 CAND_IND2 CAND_IND4 CAND_IND5 CAND_IND7 CAND_IND8 CAND_IND9) (CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9)

* Drop no packet delivered cases and a municipality that became a UyC (AYUTLA DE LOS LIBRES)
drop if TOTAL_VOTOS==0 | TOTAL_VOTOS==.

rename MUNICIPIO municipality
rename SECCION section

rename NA PANAL
rename IH PIH 
rename CG PCG 
rename ES PES

rename TOTAL_VOTOS total
rename LISTA_NOMINAL listanominal

gen   uniqueid= 0
replace uniqueid=12001 if municipality =="ACAPULCO DE JUAREZ"
replace uniqueid=12076 if municipality =="ACATEPEC"
replace uniqueid=12002 if municipality =="AHUACUOTZINGO"
replace uniqueid=12003 if municipality =="AJUCHITLAN DEL PROGRESO"
replace uniqueid=12004 if municipality =="ALCOZAUCA DE GUERRERO"
replace uniqueid=12005 if municipality =="ALPOYECA"
replace uniqueid=12006 if municipality =="APAXTLA DE CASTREJON"
replace uniqueid=12006 if municipality =="APAXTLA"
replace uniqueid=12007 if municipality =="ARCELIA"
replace uniqueid=12008 if municipality =="ATENANGO DEL RIO"
replace uniqueid=12009 if municipality =="ATLAMAJALCINGO DEL MONTE"
replace uniqueid=12010 if municipality =="ATLIXTAC"
replace uniqueid=12011 if municipality =="ATOYAC DE ALVAREZ"
replace uniqueid=12012 if municipality =="AYUTLA DE LOS LIBRES"
replace uniqueid=12013 if municipality =="AZOYU"
replace uniqueid=12014 if municipality =="BENITO JUAREZ"
replace uniqueid=12015 if municipality =="BUENAVISTA DE CUELLAR"
replace uniqueid=12028 if municipality =="CHILAPA DE ALVAREZ"
replace uniqueid=12029 if municipality =="CHILPANCINGO DE LOS BRAVO"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MARIA IZAZAGA"
replace uniqueid=12016 if municipality =="COAHUAYUTLA DE JOSE MA IZAZAGA"
replace uniqueid=12078 if municipality =="COCHOAPA EL GRANDE"
replace uniqueid=12017 if municipality =="COCULA"
replace uniqueid=12018 if municipality =="COPALA"
replace uniqueid=12019 if municipality =="COPALILLO"
replace uniqueid=12020 if municipality =="COPANATOYAC"
replace uniqueid=12021 if municipality =="COYUCA DE BENITEZ"
replace uniqueid=12022 if municipality =="COYUCA DE CATALAN"
replace uniqueid=12023 if municipality =="CUAJINICUILAPA"
replace uniqueid=12024 if municipality =="CUALAC"
replace uniqueid=12025 if municipality =="CUAUTEPEC"
replace uniqueid=12026 if municipality =="CUETZALA DEL PROGRESO"
replace uniqueid=12027 if municipality =="CUTZAMALA DE PINZON"
replace uniqueid=12075 if municipality =="EDUARDO NERI"
replace uniqueid=12030 if municipality =="FLORENCIO VILLARREAL"
replace uniqueid=12031 if municipality =="GENERAL CANUTO A. NERI"
replace uniqueid=12032 if municipality =="GENERAL HELIODORO CASTILLO"
replace uniqueid=12033 if municipality =="HUAMUXTITLAN"
replace uniqueid=12034 if municipality =="HUITZUCO DE LOS FIGUEROA"
replace uniqueid=12035 if municipality =="IGUALA DE LA INDEPENDENCIA"
replace uniqueid=12036 if municipality =="IGUALAPA"
replace uniqueid=12081 if municipality =="ILIATENCO"
replace uniqueid=12037 if municipality =="IXCATEOPAN DE CUAUHTEMOC"
replace uniqueid=12079 if municipality =="JOSE JOAQUIN DE HERRERA"
replace uniqueid=12039 if municipality =="JUAN R. ESCUDERO"
replace uniqueid=12080 if municipality =="JUCHITAN"
replace uniqueid=12068 if municipality =="LA UNION DE ISIDORO MONTES DE OCA"
replace uniqueid=12040 if municipality =="LEONARDO BRAVO"
replace uniqueid=12041 if municipality =="MALINALTEPEC"
replace uniqueid=12077 if municipality =="MARQUELIA"
replace uniqueid=12042 if municipality =="MARTIR DE CUILAPAN"
replace uniqueid=12043 if municipality =="METLATONOC"
replace uniqueid=12044 if municipality =="MOCHITLAN"
replace uniqueid=12045 if municipality =="OLINALA"
replace uniqueid=12046 if municipality =="OMETEPEC"
replace uniqueid=12047 if municipality =="PEDRO ASCENCIO ALQUISIRAS"
replace uniqueid=12048 if strpos(municipality, "PETATLAN")>0
replace uniqueid=12048 if municipality =="PETATLAN*"
replace uniqueid=12049 if municipality =="PILCAYA"
replace uniqueid=12050 if municipality =="PUNGARABATO"
replace uniqueid=12051 if municipality =="QUECHULTENANGO"
replace uniqueid=12052 if municipality =="SAN LUIS ACATLAN"
replace uniqueid=12053 if municipality =="SAN MARCOS"
replace uniqueid=12054 if municipality =="SAN MIGUEL TOTOLAPAN"
replace uniqueid=12055 if municipality =="TAXCO DE ALARCON"
replace uniqueid=12056 if municipality =="TECOANAPA"
replace uniqueid=12057 if municipality =="TECPAN DE GALEANA"
replace uniqueid=12058 if municipality =="TELOLOAPAN"
replace uniqueid=12059 if municipality =="TEPECOACUILCO DE TRUJANO"
replace uniqueid=12060 if municipality =="TETIPAC"
replace uniqueid=12061 if municipality =="TIXTLA DE GUERRERO"
replace uniqueid=12062 if municipality =="TLACOACHISTLAHUACA"
replace uniqueid=12063 if municipality =="TLACOAPA"
replace uniqueid=12064 if municipality =="TLALCHAPA"
replace uniqueid=12065 if municipality =="TLALIXTAQUILLA DE MALDONADO"
replace uniqueid=12066 if municipality =="TLAPA DE COMONFORT"
replace uniqueid=12067 if municipality =="TLAPEHUALA"
replace uniqueid=12069 if municipality =="XALPATLAHUAC"
replace uniqueid=12070 if municipality =="XOCHIHUEHUETLAN"
replace uniqueid=12071 if municipality =="XOCHISTLAHUACA"
replace uniqueid=12072 if municipality =="ZAPOTITLAN TABLAS"
replace uniqueid=12038 if municipality =="ZIHUATANEJO DE AZUETA"
replace uniqueid=12073 if municipality =="ZIRANDARO"
replace uniqueid=12074 if municipality =="ZITLALA"

collapse (sum) PAN-MORENA_ES total listanominal, by(municipality section uniqueid)

merge m:1 municipality using "coalindicators.dta"
drop _merge
erase "coalindicators.dta"

replace PRI_PVEM = PRI_PVEM + PRI + PVEM if coal_PRI_PVEM==1
replace PRI_PVEM = . if coal_PRI_PVEM==0
replace PRI=. if coal_PRI_PVEM==1
replace PVEM=. if coal_PRI_PVEM==1

rename MORENA_ES MORENA_PES
replace MORENA_PES= MORENA_PES + MORENA + PES if coal_MORENA_ES==1
replace MORENA_PES= . if coal_MORENA_ES==0
replace MORENA=.  if coal_MORENA_ES==1
replace PES=.  if coal_MORENA_ES==1

g PAN_PRD_MC2 = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if coal_PAN_PRD_MC==1
replace PAN=. if coal_PAN_PRD_MC==1
replace PRD=. if coal_PAN_PRD_MC==1
replace MC=. if coal_PAN_PRD_MC==1

g PAN_PRD2 = PAN_PRD + PAN + PRD if coal_PAN_PRD==1
replace PAN=. if coal_PAN_PRD==1
replace PRD=. if coal_PAN_PRD==1

g PRD_MC2 = PRD_MC + MC + PRD if coal_PRD_MC==1
replace MC=. if coal_PRD_MC==1
replace PRD=. if coal_PRD_MC==1

drop PAN_PRD_MC PAN_PRD PAN_MC PRD_MC
rename PAN_PRD_MC2 PAN_PRD_MC
rename PAN_PRD2 PAN_PRD
rename PRD_MC2 PRD_MC

order PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC, a(PSG)

egen valid= rowtotal (PAN PRI PRD PVEM PT MC PANAL MORENA PES PPG PIH PCG PSM PSG PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9)

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PPG PIH PCG PSM PSG PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_PPG i_PIH i_PCG i_PSM i_PSG i_PAN_PRD i_PRD_MC i_MORENA_PES i_PRI_PVEM i_PAN_PRD_MC i_CI_1 i_CI_2 i_CI_4 i_CI_5 i_CI_7 i_CI_8 i_CI_9, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r PPG_r PIH_r PCG_r PSM_r PSG_r PAN_PRD_r PRD_MC_r MORENA_PES_r PRI_PVEM_r PAN_PRD_MC_r CI_1_r CI_2_r CI_4_r CI_5_r CI_7_r CI_8_r CI_9_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PPG PIH PCG PSM PSG PAN_PRD PRD_MC MORENA_PES PRI_PVEM PAN_PRD_MC CI_1 CI_2 CI_4 CI_5 CI_7 CI_8 CI_9 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

merge m:1 uniqueid using incumbents2018.dta
drop if _merge==2
drop _merge
erase incumbents2018.dta

gen incumbent_vote = .

egen maxmc = rowmax(MC PRD_MC PAN_PRD_MC)
replace incumbent_vote=maxmc if incumbent=="MC" 

egen maxpan = rowmax(PAN PAN_PRD PAN_PRD_MC)
replace incumbent_vote=maxpan if incumbent=="PAN"

replace incumbent_vote=PANAL if incumbent=="PANAL" 

replace incumbent_vote=PPG if incumbent=="PPG"

egen maxprd = rowmax(PRD PAN_PRD PAN_PRD_MC)
replace incumbent_vote=maxprd if incumbent=="PRD"

egen maxpri = rowmax(PRI PRI_PVEM)
replace incumbent_vote=maxpri if incumbent=="PRI"

replace incumbent_vote=PT if incumbent=="PT"

egen maxpvem = rowmax(PVEM PRI_PVEM)
replace incumbent_vote=maxpvem if incumbent=="PVEM"

egen maxpripvem = rowmax(PRI PRI_PVEM PVEM)
replace incumbent_vote=maxpripvem if incumbent=="PRI_PVEM"

egen maxpripvempanal = rowmax(PRI PRI_PVEM PVEM PANAL)
replace incumbent_vote=maxpripvempanal if incumbent=="PRI_PVEM_PANAL"

egen maxprdpt = rowmax(PRD PAN_PRD PAN_PRD_MC PT)
replace incumbent_vote=maxprdpt if incumbent=="PRD_PT"

replace incumbent_vote = . if municipality=="LEONARDO BRAVO"

drop max* coal*

g year=2018
g month="July"
g STATE="GUERRERO"

save Guerrero_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent, by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Guerrero_2018.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************

clear
append using Guerrero_Section_2015.dta
append using Guerrero_Section_2018.dta

erase Guerrero_Section_2015.dta
erase Guerrero_Section_2018.dta

g PAN_winner = 0
replace PAN_winner = 1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
g winner_counter = PAN_winner

foreach var in PRI PRD PT MC PVEM PANAL PPG MORENA PES {
	g `var'_winner = 0
	replace `var'_winner = 1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0
drop winner_counter

save Guerrero_Section_15_18.dta, replace

use "..\..\Precinct\Guerrero_ALL.dta", clear
append using Guerrero_Section_15_18.dta

erase Guerrero_Section_15_18.dta

save Guerrero_ALL_SALVADOR.dta, replace
