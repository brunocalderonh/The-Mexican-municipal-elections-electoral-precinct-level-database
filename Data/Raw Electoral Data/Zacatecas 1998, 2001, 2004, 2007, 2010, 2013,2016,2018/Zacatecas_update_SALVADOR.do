////////////////////////////////////////////////
////////ZACATECAS PRECINCT PANEL UPDATE/////////
////////JUL 2019////////////////////////////////
////////SALVADOR ASCENCIO///////////////////////
////////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Zacatecas"

//////////////////////////////////
////////////2016//////////////////
//////////////////////////////////

import excel "Ayuntamientos_2016.xlsx", clear firstrow 

drop uniqueid
gen   uniqueid= 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUÁREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CA+ITAS DE FELIPE PESCADOR"
replace uniqueid=32006 if strpos(municipality, "ITAS DE FELIPE PESCADOR")>0
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCIÓN DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTÉMOC"
replace uniqueid=32015 if municipality =="EL PLATEADO DE JOAQUÍN AMARO"
replace uniqueid=32041 if municipality =="EL SALVADOR"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="GENERAL ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="GENERAL FRANCISCO R. MURGUÍA"
replace uniqueid=32016 if municipality =="GENERAL PÁNFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMÉNEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUIS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLÁN DE MEJÍA"
replace uniqueid=32035 if municipality =="NORIA DE ÁNGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PÁNUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RÍO GRANDE"
replace uniqueid=32040 if municipality =="SAIN ALTO"
replace uniqueid=32058 if municipality =="SANTA MARÍA DE LA PAZ"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACÁN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLÁN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZÁLEZ ORTEGA"
replace uniqueid=32048 if municipality =="TLALTENANGO DE SÁNCHEZ ROMÁN"
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCÍA DE LA CADENA"
replace uniqueid=32049 if municipality =="VALPARAÍSO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCÍA"
replace uniqueid=32053 if municipality =="VILLA GONZÁLEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

order uniqueid, a(municipality)

drop if Dtto==""

destring *, replace

collapse (sum) listanominal-nulo, by(municipality uniqueid section)

egen CI_1=rowtotal(CI1-CI18)
egen CI_2= rowtotal(CI9 CI14 CI18)
gen CI_3=CI13
replace CI_1=CI_1-CI_2-CI_3
drop CI1-CI18

foreach var in PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PVEM_PANAL {
	bys uniqueid: egen indmun_`var'= sum(`var') 
}

replace PAN_PRD=PAN_PRD+PAN+PRD
drop PAN PRD

replace PRI_PVEM_PANAL=PRI_PVEM_PANAL+ PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if indmun_PRI_PVEM_PANAL!=0
replace PRI=. if indmun_PRI_PVEM_PANAL!=0
replace PVEM=. if indmun_PRI_PVEM_PANAL!=0
replace PANAL=. if indmun_PRI_PVEM_PANAL!=0
replace PRI_PVEM=. if indmun_PRI_PVEM_PANAL!=0
replace PVEM_PANAL=. if indmun_PRI_PVEM_PANAL!=0
drop PRI_PANAL PVEM_PANAL

replace PRI_PVEM=PRI + PVEM + PRI_PVEM if (indmun_PRI_PVEM_PANAL==0 & indmun_PRI_PVEM!=0)
replace PRI=.  if (indmun_PRI_PVEM_PANAL==0 & indmun_PRI_PVEM!=0)
replace PVEM=.  if (indmun_PRI_PVEM_PANAL==0 & indmun_PRI_PVEM!=0)

drop PRI PVEM

drop indmun*

egen total = rowtotal(PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL PRI_PVEM CI_1 CI_2 CI_3 no_reg nulo)
egen valid=rowtotal(PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL PRI_PVEM CI_1 CI_2 CI_3)

drop nulo no_reg

foreach var in PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL PRI_PVEM CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

gen turnout=total/listanominal
gen mun_turnout=mun_total/mun_listanominal

rowranks i_PT i_MC i_PANAL i_MORENA i_PES i_PAN_PRD i_PRI_PVEM_PANAL i_PRI_PVEM i_CI_1 i_CI_2 i_CI_3, gen(PT_r MC_r PANAL_r MORENA_r PES_r PAN_PRD_r PRI_PVEM_PANAL_r PRI_PVEM_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PT MC PANAL MORENA PES PAN_PRD PRI_PVEM_PANAL PRI_PVEM CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2" | winner=="CI_3"
replace second="Independent" if second=="CI_1" | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2" | third=="CI_3"

g year=2016
g month="June"
g STATE="ZACATECAS"

save Zacatecas_Section_2016.dta, replace

collapse (first) winner, by (municipality uniqueid)
* special election winner was instead
replace winner = "PRI_PVEM_PANAL" if municipality=="ZACATECAS"
rename winner incumbent

save incumbents18.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Zacatecas_Section_2016.dta
collapse (sum) listanominal-total (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Zacatecas_2016.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////////////////////
////////////2016 EXTRAORDINARIO //////////////////
//////////////////////////////////////////////////

import excel "Eleccion_ext_2016_CON_CASILLAS CON LISTA NOMINAL.xlsx", sheet("Ayuntamiento") cellrange(A2:Z200) firstrow clear

rename Sección section
drop if section=="Total"
g uniqueid = 32056 
g municipality = "ZACATECAS EXTRAORDINARIO"
rename LN listanominal
rename VotaciónTotal total

destring _all, replace

g PAN_PRD_PT = D + F + G + L + M + N + O
g PRI_PVEM_PANAL = E + H + J + P + Q + R + S
rename I MC
rename K PES
rename T CI_1
rename U CI_3
rename V CI_2

collapse (sum) PAN_PRD_PT PRI_PVEM_PANAL MC PES CI_1 CI_2 CI_3 listanominal total, by(municipality uniqueid section)

egen valid = rowtotal(PAN_PRD_PT PRI_PVEM_PANAL MC PES CI_1 CI_2 CI_3)

foreach var in PAN_PRD_PT PRI_PVEM_PANAL MC PES CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN_PRD_PT i_PRI_PVEM_PANAL i_MC i_PES i_CI_1 i_CI_2 i_CI_3, gen(PAN_PRD_PT_r PRI_PVEM_PANAL_r MC_r PES_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD_PT PRI_PVEM_PANAL MC PES CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"  | winner=="CI_3"
replace second="Independent" if second=="CI_1"  | second=="CI_2"| second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2" | third=="CI_3"

gen year=2016
gen month="December"
gen STATE="ZACATECAS"

save Zacatecas_Section_2016_Extraordinario.dta, replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2018//////////////////
//////////////////////////////////

import excel "Ayuntamientos_2018.xlsx", clear firstrow

drop uniqueid
g uniqueid = 0
replace uniqueid=32001 if municipality =="APOZOL"
replace uniqueid=32002 if municipality =="APULCO"
replace uniqueid=32003 if municipality =="ATOLINGA"
replace uniqueid=32004 if municipality =="BENITO JUÃREZ"
replace uniqueid=32005 if municipality =="CALERA"
replace uniqueid=32006 if municipality =="CAÃ‘ITAS "
replace uniqueid=32009 if municipality =="CHALCHIHUITES"
replace uniqueid=32007 if municipality =="CONCEPCIÃ“N DEL ORO"
replace uniqueid=32008 if municipality =="CUAUHTÃ‰MOC"
replace uniqueid=32015 if municipality =="EL PLATEADO "
replace uniqueid=32041 if municipality =="EL SALVADOR"
replace uniqueid=32010 if municipality =="FRESNILLO"
replace uniqueid=32012 if municipality =="GENARO CODINA"
replace uniqueid=32013 if municipality =="ENRIQUE ESTRADA"
replace uniqueid=32014 if municipality =="FRANCISCO R. MURGUIA"
replace uniqueid=32016 if municipality =="PÃNFILO NATERA"
replace uniqueid=32017 if municipality =="GUADALUPE"
replace uniqueid=32018 if municipality =="HUANUSCO"
replace uniqueid=32019 if municipality =="JALPA"
replace uniqueid=32020 if municipality =="JEREZ"
replace uniqueid=32021 if municipality =="JIMÃ‰NEZ DEL TEUL"
replace uniqueid=32022 if municipality =="JUAN ALDAMA"
replace uniqueid=32023 if municipality =="JUCHIPILA"
replace uniqueid=32024 if municipality =="LORETO"
replace uniqueid=32025 if municipality =="LUÃS MOYA"
replace uniqueid=32026 if municipality =="MAZAPIL"
replace uniqueid=32027 if municipality =="MELCHOR OCAMPO"
replace uniqueid=32028 if municipality =="MEZQUITAL DEL ORO"
replace uniqueid=32029 if municipality =="MIGUEL AUZA"
replace uniqueid=32030 if municipality =="MOMAX"
replace uniqueid=32031 if municipality =="MONTE ESCOBEDO"
replace uniqueid=32032 if municipality =="MORELOS"
replace uniqueid=32033 if municipality =="MOYAHUA DE ESTRADA"
replace uniqueid=32034 if municipality =="NOCHISTLÃN DE MEJÃA"
replace uniqueid=32035 if municipality =="NORIA DE ÃNGELES"
replace uniqueid=32036 if municipality =="OJOCALIENTE"
replace uniqueid=32037 if municipality =="PÃNUCO"
replace uniqueid=32038 if municipality =="PINOS"
replace uniqueid=32039 if municipality =="RÃO GRANDE"
replace uniqueid=32040 if municipality =="SAÃN ALTO"
replace uniqueid=32058 if municipality =="SANTA MARÃA DE LA PAZ"
replace uniqueid=32042 if municipality =="SOMBRERETE"
replace uniqueid=32043 if municipality =="SUSTICACÃN"
replace uniqueid=32044 if municipality =="TABASCO"
replace uniqueid=32045 if municipality =="TEPECHITLÃN"
replace uniqueid=32046 if municipality =="TEPETONGO"
replace uniqueid=32047 if municipality =="TEUL DE GONZÃLEZ "
replace uniqueid=32048 if municipality =="TLALTENANGO "
replace uniqueid=32057 if municipality =="TRANCOSO"
replace uniqueid=32011 if municipality =="TRINIDAD GARCÃA"
replace uniqueid=32049 if municipality =="VALPARAISO"
replace uniqueid=32050 if municipality =="VETAGRANDE"
replace uniqueid=32051 if municipality =="VILLA DE COS"
replace uniqueid=32052 if municipality =="VILLA GARCÃA"
replace uniqueid=32053 if municipality =="VILLA GONZÃLEZ ORTEGA"
replace uniqueid=32054 if municipality =="VILLA HIDALGO"
replace uniqueid=32055 if municipality =="VILLANUEVA"
replace uniqueid=32056 if municipality =="ZACATECAS"

order uniqueid, a(municipality)

rename *ES* *PES*

rename NA PANAL

replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC
drop PAN_PRD PAN_MC PRD_MC PAN PRD MC

replace PT_MORENA_PES=PT_MORENA_PES+ PT_MORENA + PT_PES + MORENA_PES + MORENA + PES + PT if municipality!="LORETO"
replace PT=. if municipality!="LORETO"
replace PES=. if municipality!="LORETO"
replace MORENA=. if municipality!="LORETO"
drop PT_MORENA PT_PES MORENA_PES

egen CI_1= rowtotal(CAND_IND2 - CAN_IND7)
rename CAND_IND4 CI_2
replace CI_1=CI_1 - CI_2
drop CAND_IND* CAN_IND7

collapse (sum) PRI-CI_1, by (STATE municipality uniqueid section)

drop NUM_VOTOS_VALIDOS total
order CI_2 listanominal, a(CI_1)

egen total = rowtotal(PRI PT PVEM PANAL MORENA PES PAZ MDZ PP PAN_PRD_MC PT_MORENA_PES CI_1 CI_2 no_reg nulo)
drop nulo no_reg 

egen valid = rowtotal(PRI PT PVEM PANAL MORENA PES PAZ MDZ PP PAN_PRD_MC PT_MORENA_PES CI_1 CI_2)

foreach var in PRI PT PVEM PANAL MORENA PES PAZ MDZ PP PAN_PRD_MC PT_MORENA_PES CI_1 CI_2 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PRI i_PT i_PVEM i_PANAL i_MORENA i_PES i_PAZ i_MDZ i_PP i_PAN_PRD_MC i_PT_MORENA_PES i_CI_1 i_CI_2, gen(PRI_r PT_r PVEM_r PANAL_r MORENA_r PES_r PAZ_r MDZ_r PP_r PAN_PRD_MC_r PT_MORENA_PES_r CI_1_r CI_2_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PT PVEM PANAL MORENA PES PAZ MDZ PP PAN_PRD_MC PT_MORENA_PES CI_1 CI_2 {
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

merge m:1 uniqueid using incumbents18.dta
drop _merge
erase incumbents18.dta
replace incumbent = "PRI_PVEM_PANAL" if municipality=="ZACATECAS"

gen incumbent_vote=.

replace incumbent_vote=PRI if incumbent=="PRI_PVEM" | incumbent=="PRI_PVEM_PANAL" 

replace incumbent_vote=PANAL if incumbent=="PANAL" 

replace incumbent_vote=PAN_PRD_MC if incumbent=="PAN_PRD"

replace incumbent_vote=PAN_PRD_MC if incumbent=="MC"

replace incumbent_vote=MORENA if incumbent=="MORENA"

replace incumbent_vote=PT if incumbent=="PT"

replace incumbent_vote=PT_MORENA_PES if (incumbent=="MORENA" | incumbent=="PT" ) & PT_MORENA_PES!=0

save Zacatecas_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Zacatecas_Section_2018.dta
collapse (sum) PRI-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Zacatecas_2018.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

clear
append using Zacatecas_Section_2016.dta
append using Zacatecas_Section_2016_Extraordinario.dta
append using Zacatecas_Section_2018.dta

erase Zacatecas_Section_2016.dta
erase Zacatecas_Section_2016_Extraordinario.dta
erase Zacatecas_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC PAZ {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
drop winner_counter

save Zacatecas_Section_16_18.dta, replace

use "..\..\Precinct\Zacatecas_ALL.dta", clear
append using Zacatecas_Section_16_18.dta

erase Zacatecas_Section_16_18.dta

save Zacatecas_ALL_SALVADOR.dta, replace 
