////////////////////////////////////////////
////////SLP PRECINCT PANEL UPDATE///////////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

clear
cd "D:\Dropbox\Salvador Ascencio\Update Precincts\San Luis Potosi"

import excel using "Ayuntamientos_2015.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2015.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if Seccion==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "AHUALULCO.dta"
append using "ALAQUINES.dta"
append using "AQUISMON.dta"
append using "ARMADILLO DE LOS INFANTE.dta"
append using "VILLA DE ARRIAGA.dta"
append using "AXTLA DE TERRAZAS.dta"
append using "CARDENAS.dta"
append using "CATORCE.dta"
append using "CEDRAL.dta"
append using "CERRITOS.dta"
append using "CHARCAS.dta"
append using "CIUDAD DEL MAIZ.dta"
append using "CIUDAD FERNANDEZ.dta"
append using "COXCATLAN.dta"
append using "EBANO.dta"
append using "EL NARANJO.dta"
append using "GUADALCAZAR.dta"
append using "HUEHUETLÁN.dta"
append using "LAGUNILLAS.dta"
append using "MATEHUALA.dta"
append using "MATLAPA.dta"
append using "MEXQUITIC.dta"
append using "MOCTEZUMA.dta"
append using "RAYON.dta"
append using "RIOVERDE.dta"
append using "SALINAS.dta"
append using "SAN ANTONIO.dta"
append using "SAN CIRO DE ACOSTA.dta"
append using "SAN MARTIN CHALCH.dta"
append using "SAN NICOLAS TOLENTINO.dta"
append using "SAN VICENTE TANCUAYALAB.dta"
append using "CERRO DE SAN PEDRO.dta"
append using "SANTA CATARINA.dta"
append using "SANTA MARIA DEL RIO.dta"
append using "SANTO DOMINGO.dta"
append using "SAN LUIS POTOSI.dta"
append using "SOLEDAD.dta"
append using "TAMASOPO.dta"
append using "TAMAZUNCHALE.dta"
append using "TAMPACAN.dta"
append using "TAMPAMOLON CORONA.dta"
append using "TAMUIN.dta"
append using "TANCANHUITZ.dta"
append using "TANLAJAS.dta"
append using "TANQUIAN DE ESCOBEDO.dta"
append using "TIERRA NUEVA.dta"
append using "CIUDAD VALLES.dta"
append using "VANEGAS.dta"
append using "VENADO.dta"
append using "VILLA DE ARISTA.dta"
append using "VILLA DE LA PAZ.dta"
append using "VILLA DE RAMOS.dta"
append using "VILLA DE REYES.dta"
append using "VILLA DE GUADALUPE.dta"
append using "VILLA HIDALGO.dta"
append using "VILLA JUAREZ.dta"
append using "XILITLA.dta"
append using "ZARAGOZA.dta"

drop if Municipio==""

drop EXCEDENODEBOLETAS PorcentajedeVotacionEmitida PorcentajedeVotosNulos VOTACIONNULAMAYORAL5 CAPTURADA VOTACIONVALIDAEMITIDA

destring *, replace 

rename Municipio municipality
gen   uniqueid= 0
replace uniqueid=24001 if strpos(municipality, "AHUALULCO")>0
replace uniqueid=24002 if strpos(municipality, "ALAQUINES")>0
replace uniqueid=24003 if strpos(municipality, "AQUISMON")>0
replace uniqueid=24004 if strpos(municipality, "ARMADILLO DE LOS INFANTE")>0
replace uniqueid=24053 if strpos(municipality, "AXTLA DE TERRAZAS")>0
replace uniqueid=24005 if strpos(municipality, "CARDENAS")>0
replace uniqueid=24006 if strpos(municipality, "CATORCE")>0
replace uniqueid=24007 if strpos(municipality, "CEDRAL")>0
replace uniqueid=24008 if strpos(municipality, "CERRITOS")>0
replace uniqueid=24009 if strpos(municipality, "CERRO DE SAN PEDRO")>0
replace uniqueid=24015 if strpos(municipality, "CHARCAS")>0
replace uniqueid=24010 if strpos(municipality, "CIUDAD DEL MAIZ")>0
replace uniqueid=24011 if strpos(municipality, "CIUDAD FERNANDEZ")>0
replace uniqueid=24013 if strpos(municipality, "CIUDAD VALLES")>0
replace uniqueid=24014 if strpos(municipality, "COXCATLAN")>0
replace uniqueid=24016 if strpos(municipality, "EBANO")>0
replace uniqueid=24058 if strpos(municipality, "EL NARANJO")>0
replace uniqueid=24017 if strpos(municipality, "GUADALCAZAR")>0
replace uniqueid=24018 if strpos(municipality, "HUEHUETLAN")>0
replace uniqueid=24019 if strpos(municipality, "LAGUNILLAS")>0
replace uniqueid=24020 if strpos(municipality, "MATEHUALA")>0
replace uniqueid=24057 if strpos(municipality, "MATLAPA")>0
replace uniqueid=24021 if strpos(municipality, "MEXQUITIC DE CARMONA")>0
replace uniqueid=24022 if strpos(municipality, "MOCTEZUMA")>0
replace uniqueid=24023 if strpos(municipality, "RAYON")>0
replace uniqueid=24024 if strpos(municipality, "RIOVERDE")>0
replace uniqueid=24025 if strpos(municipality, "SALINAS")>0
replace uniqueid=24026 if strpos(municipality, "SAN ANTONIO")>0
replace uniqueid=24027 if strpos(municipality, "SAN CIRO DE ACOSTA")>0
replace uniqueid=24028 if strpos(municipality, "SAN LUIS POTOSI")>0
replace uniqueid=24029 if strpos(municipality, "SAN MARTIN CHALCHICUAUTLA")>0
replace uniqueid=24030 if strpos(municipality, "SAN NICOLAS TOLENTINO")>0
replace uniqueid=24034 if strpos(municipality, "SAN VICENTE TANCUAYALAB")>0
replace uniqueid=24031 if strpos(municipality, "SANTA CATARINA")>0
replace uniqueid=24032 if strpos(municipality, "SANTA MARIA DEL RIO")>0
replace uniqueid=24033 if strpos(municipality, "SANTO DOMINGO")>0
replace uniqueid=24035 if strpos(municipality, "SOLEDAD DE GRACIANO SANCHEZ")>0
replace uniqueid=24036 if strpos(municipality, "TAMASOPO")>0
replace uniqueid=24037 if strpos(municipality, "TAMAZUNCHALE")>0
replace uniqueid=24038 if strpos(municipality, "TAMPACAN")>0
replace uniqueid=24039 if strpos(municipality, "TAMPAMOLON CORONA")>0
replace uniqueid=24040 if strpos(municipality, "TAMUIN")>0
replace uniqueid=24012 if strpos(municipality, "TANCANHUITZ")>0
replace uniqueid=24041 if strpos(municipality, "TANLAJAS")>0
replace uniqueid=24042 if strpos(municipality, "TANQUIAN DE ESCOBEDO")>0
replace uniqueid=24043 if strpos(municipality, "TIERRA NUEVA")>0
replace uniqueid=24044 if strpos(municipality, "VANEGAS")>0
replace uniqueid=24045 if strpos(municipality, "VENADO")>0
replace uniqueid=24056 if strpos(municipality, "VILLA DE ARISTA")>0
replace uniqueid=24046 if strpos(municipality, "VILLA DE ARRIAGA")>0
replace uniqueid=24047 if strpos(municipality, "VILLA DE GUADALUPE")>0
replace uniqueid=24048 if strpos(municipality, "VILLA DE LA PAZ")>0
replace uniqueid=24049 if strpos(municipality, "VILLA DE RAMOS")>0
replace uniqueid=24050 if strpos(municipality, "VILLA DE REYES")>0
replace uniqueid=24051 if strpos(municipality, "VILLA HIDALGO")>0
replace uniqueid=24052 if strpos(municipality, "VILLA JUAREZ")>0
replace uniqueid=24054 if strpos(municipality, "XILITLA")>0
replace uniqueid=24055 if strpos(municipality, "ZARAGOZA")>0

rename PNA PANAL
rename *_PNA* *_PANAL*
rename PMC MC
rename PMC* MC*
rename *_PMC* *_MC*

rename (Seccion ListaNominal FORMULASNOREGISTRADAS VOTOSNULOS VOTACIONEMITIDA CI) (section listanominal no_reg nulo total CI_1)

collapse (sum) listanominal total PCP MORENA PAN_MC-PVEM_PANAL, by(municipality section uniqueid)

egen valid = rowtotal(PCP MORENA PAN_MC PRI_PVEM PRD_PT_PANAL PAN_PT PRI_PANAL MC PAN_PRD_PT PRI_PVEM_PANAL PAN PRI PH PES PAN_PRD_PT_PVEM_MC_PANAL PRD PANAL PAN_PT_MC CI_1 PRD_PCP PVEM PT PRD_PT_PCP ///
	PRD_MC PT_MC PRD_PT PRD_PT_MC PRD_PT_PCP_MC PAN_PCP PRD_PT_MC_PANAL PAN_PVEM_PANAL PRD_PVEM_PCP PCP_MC_PANAL PAN_PRD_PT_MC PRD_PCP_MC PVEM_MC PCP_PANAL MC_PANAL PVEM_PANAL)

foreach var in PCP MORENA PAN_MC PRI_PVEM PRD_PT_PANAL PAN_PT PRI_PANAL MC PAN_PRD_PT PRI_PVEM_PANAL PAN PRI PH PES PAN_PRD_PT_PVEM_MC_PANAL PRD PANAL PAN_PT_MC CI_1 PRD_PCP PVEM PT PRD_PT_PCP ///
	PRD_MC PT_MC PRD_PT PRD_PT_MC PRD_PT_PCP_MC PAN_PCP PRD_PT_MC_PANAL PAN_PVEM_PANAL PRD_PVEM_PCP PCP_MC_PANAL PAN_PRD_PT_MC PRD_PCP_MC PVEM_MC PCP_PANAL MC_PANAL PVEM_PANAL total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = valid/listanominal
g mun_turnout = mun_valid/mun_listanominal

rowranks i_PCP i_MORENA i_PAN_MC i_PRI_PVEM i_PRD_PT_PANAL i_PAN_PT i_PRI_PANAL i_MC i_PAN_PRD_PT i_PRI_PVEM_PANAL i_PAN i_PRI i_PH i_PES i_PAN_PRD_PT_PVEM_MC_PANAL i_PRD i_PANAL i_PAN_PT_MC i_CI_1 i_PRD_PCP i_PVEM i_PT i_PRD_PT_PCP ///
	i_PRD_MC i_PT_MC i_PRD_PT i_PRD_PT_MC i_PRD_PT_PCP_MC i_PAN_PCP i_PRD_PT_MC_PANAL i_PAN_PVEM_PANAL i_PRD_PVEM_PCP i_PCP_MC_PANAL i_PAN_PRD_PT_MC i_PRD_PCP_MC i_PVEM_MC i_PCP_PANAL i_MC_PANAL i_PVEM_PANAL, ///
	gen(PCP_r MORENA_r PAN_MC_r PRI_PVEM_r PRD_PT_PANAL_r PAN_PT_r PRI_PANAL_r MC_r PAN_PRD_PT_r PRI_PVEM_PANAL_r PAN_r PRI_r PH_r PES_r PAN_PRD_PT_PVEM_MC_PANAL_r PRD_r PANAL_r PAN_PT_MC_r CI_1_r PRD_PCP_r PVEM_r PT_r PRD_PT_PCP_r ///
	PRD_MC_r PT_MC_r PRD_PT_r PRD_PT_MC_r PRD_PT_PCP_MC_r PAN_PCP_r PRD_PT_MC_PANAL_r PAN_PVEM_PANAL_r PRD_PVEM_PCP_r PCP_MC_PANAL_r PAN_PRD_PT_MC_r PRD_PCP_MC_r PVEM_MC_r PCP_PANAL_r MC_PANAL_r PVEM_PANAL_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PCP MORENA PAN_MC PRI_PVEM PRD_PT_PANAL PAN_PT PRI_PANAL MC PAN_PRD_PT PRI_PVEM_PANAL PAN PRI PH PES PAN_PRD_PT_PVEM_MC_PANAL PRD PANAL PAN_PT_MC CI_1 PRD_PCP PVEM PT PRD_PT_PCP ///
	PRD_MC PT_MC PRD_PT PRD_PT_MC PRD_PT_PCP_MC PAN_PCP PRD_PT_MC_PANAL PAN_PVEM_PANAL PRD_PVEM_PCP PCP_MC_PANAL PAN_PRD_PT_MC PRD_PCP_MC PVEM_MC PCP_PANAL MC_PANAL PVEM_PANAL {
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
g STATE="SAN LUIS POTOSI"

save San_Luis_Potosi_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent

save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use San_Luis_Potosi_Section_2015.dta
collapse (sum) listanominal-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\San_Luis_Potosi_2015.dta", replace

**********************************************************************
**********************************************************************

erase "AHUALULCO.dta"
erase "ALAQUINES.dta"
erase "AQUISMON.dta"
erase "ARMADILLO DE LOS INFANTE.dta"
erase "VILLA DE ARRIAGA.dta"
erase "AXTLA DE TERRAZAS.dta"
erase "CARDENAS.dta"
erase "CATORCE.dta"
erase "CEDRAL.dta"
erase "CERRITOS.dta"
erase "CHARCAS.dta"
erase "CIUDAD DEL MAIZ.dta"
erase "CIUDAD FERNANDEZ.dta"
erase "COXCATLAN.dta"
erase "EBANO.dta"
erase "EL NARANJO.dta"
erase "GUADALCAZAR.dta"
erase "HUEHUETLÁN.dta"
erase "LAGUNILLAS.dta"
erase "MATEHUALA.dta"
erase "MATLAPA.dta"
erase "MEXQUITIC.dta"
erase "MOCTEZUMA.dta"
erase "RAYON.dta"
erase "RIOVERDE.dta"
erase "SALINAS.dta"
erase "SAN ANTONIO.dta"
erase "SAN CIRO DE ACOSTA.dta"
erase "SAN MARTIN CHALCH.dta"
erase "SAN NICOLAS TOLENTINO.dta"
erase "SAN VICENTE TANCUAYALAB.dta"
erase "CERRO DE SAN PEDRO.dta"
erase "SANTA CATARINA.dta"
erase "SANTA MARIA DEL RIO.dta"
erase "SANTO DOMINGO.dta"
erase "SAN LUIS POTOSI.dta"
erase "SOLEDAD.dta"
erase "TAMASOPO.dta"
erase "TAMAZUNCHALE.dta"
erase "TAMPACAN.dta"
erase "TAMPAMOLON CORONA.dta"
erase "TAMUIN.dta"
erase "TANCANHUITZ.dta"
erase "TANLAJAS.dta"
erase "TANQUIAN DE ESCOBEDO.dta"
erase "TIERRA NUEVA.dta"
erase "CIUDAD VALLES.dta"
erase "VANEGAS.dta"
erase "VENADO.dta"
erase "VILLA DE ARISTA.dta"
erase "VILLA DE LA PAZ.dta"
erase "VILLA DE RAMOS.dta"
erase "VILLA DE REYES.dta"
erase "VILLA DE GUADALUPE.dta"
erase "VILLA HIDALGO.dta"
erase "VILLA JUAREZ.dta"
erase "XILITLA.dta"
erase "ZARAGOZA.dta"

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

///////////
////2018///
///////////

import excel using "MunicipiosSLP_2018.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "MunicipiosSLP_2018.xlsx", sheet("`sheetname'") clear firstrow allstring
	gen municipality="`sheetname'"
	drop if CASILLAACTA==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "Ahualulco.dta"
append using "Alaquines.dta"
append using "Aquismon.dta"
append using "Armadillo.dta"
append using "Cardenas.dta"
append using "Catorce.dta"
append using "Cedral.dta"
append using "Cerritos.dta"
append using "Cerro de San Pedro.dta"
append using "Ciudad del Maiz.dta"
append using "Ciudad Fernandez.dta"
append using "Ciudad Valles.dta"
append using "Tancanhuitz.dta"
append using "Coxcatlan.dta"
append using "Charcas.dta"
append using "Ebano.dta"
append using "Guadalzacar.dta"
append using "Huhuetlan.dta"
append using "Lagunillas.dta"
append using "Matehuala.dta"
append using "Mexquitic.dta"
append using "Moctezuma.dta"
append using "Rayon.dta"
append using "Rioverde.dta"
append using "Salinas.dta"
append using "San Antonio.dta"
append using "San Ciro.dta"
append using "San Luis.dta"
append using "San Martin.dta"
append using "San Nicolas.dta"
append using "San Vicente.dta"
append using "Sta Catarina.dta"
append using "Sta Maria.dta"
append using "Sto Domingo.dta"
append using "Soledad G S.dta"
append using "Tamasopo.dta"
append using "Tamazunchale.dta"
append using "Tampacan.dta"
append using "Tampamolon.dta"
append using "Tamuin.dta"
append using "Tanlajas.dta"
append using "Tanquian.dta"
append using "Tierra Nueva.dta"
append using "Vanegas.dta"
append using "Venado.dta"
append using "Villa de Arista.dta"
append using "Villa de Arriaga.dta"
append using "Villa de Guadalupe.dta"
append using "Villa de la Paz.dta"
append using "Villa de Ramos.dta"
append using "Villa de Reyes.dta"
append using "Villa Hidalgo.dta"
append using "Villa Juarez.dta"
append using "Axtla.dta"
append using "Xilitla.dta"
append using "Zaragoza.dta"
append using "El Naranjo.dta"
append using "Matlapa.dta"

drop O P section

g SECCION=substr(CASILLAACTA,1,4)

destring *, replace

replace PTMORENAPES = 0 in 3011

order municipality SECCION obs CASILLAACTA *

rename *PMC *MC
rename *PNA *PANAL
rename *PNA* *PANAL*

preserve
import excel using "uniqueids.xlsx", first clear
save uniqueids.dta, replace
restore

merge m:1 municipality using uniqueids.dta
drop municipality _merge
erase uniqueids.dta

rename municipality2 municipality

replace PANPRDMC = PANPRDMC + PANPRD + PRDMC + PANMC + PAN + PRD + MC if PANPRDMC!=.
replace PANPRD=. if PANPRDMC!=.
replace PRDMC=. if PANPRDMC!=.
replace PANMC=. if PANPRDMC!=.
replace MC=. if PANPRDMC!=.
replace PAN=. if PANPRDMC!=.
replace PRD=. if PANPRDMC!=.

drop PANPRD PRDMC

rename (PANPRDMC PANMC) (PAN_PRD_MC PAN_MC)

replace PTMORENAPES = PTMORENAPES + PTMORENA + PTPES + MORENAPES + PT + MORENA + PES if PTMORENAPES!=.
replace PT=. if  PTMORENAPES!=. 
replace MORENA=. if  PTMORENAPES!=. 
replace PES=. if  PTMORENAPES!=. 
drop PTMORENA PTPES MORENAPES

drop PES

rename PTMORENAPES PT_MORENA_PES

rename (PRIPVEMPCP PRIPVEMPANAL PVEMPANAL PRIPVEM PRIPANAL PRIPANALPCP PRIPVEMPANALPCP PRIPCP) (PRI_PVEM_PCP PRI_PVEM_PANAL PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP)

rename CI CI_1

drop uniqueid
gen   uniqueid= 0
replace uniqueid=24001 if strpos(municipality, "AHUALULCO")>0
replace uniqueid=24002 if strpos(municipality, "ALAQUINES")>0
replace uniqueid=24003 if strpos(municipality, "AQUISMÓN")>0
replace uniqueid=24004 if strpos(municipality, "ARMADILLO DE LOS INFANTE")>0
replace uniqueid=24053 if strpos(municipality, "AXTLA DE TERRAZAS")>0
replace uniqueid=24005 if strpos(municipality, "CÁRDENAS")>0
replace uniqueid=24006 if strpos(municipality, "CATORCE")>0
replace uniqueid=24007 if strpos(municipality, "CEDRAL")>0
replace uniqueid=24008 if strpos(municipality, "CERRITOS")>0
replace uniqueid=24009 if strpos(municipality, "CERRO DE SAN PEDRO")>0
replace uniqueid=24015 if strpos(municipality, "CHARCAS")>0
replace uniqueid=24010 if strpos(municipality, "CIUDAD DEL MAÍZ")>0
replace uniqueid=24011 if strpos(municipality, "CIUDAD FERNÁNDE")>0
replace uniqueid=24013 if strpos(municipality, "CIUDAD VALLES")>0
replace uniqueid=24014 if strpos(municipality, "COXCATLÁN")>0
replace uniqueid=24016 if strpos(municipality, "EBANO")>0
replace uniqueid=24058 if strpos(municipality, "EL NARANJO")>0
replace uniqueid=24017 if strpos(municipality, "GUADALCÁZAR")>0
replace uniqueid=24018 if strpos(municipality, "HUEHUETLÁN")>0
replace uniqueid=24019 if strpos(municipality, "LAGUNILLAS")>0
replace uniqueid=24020 if strpos(municipality, "MATEHUALA")>0
replace uniqueid=24057 if strpos(municipality, "MATLAPA")>0
replace uniqueid=24021 if strpos(municipality, "MEXQUITIC DE CARMONA")>0
replace uniqueid=24022 if strpos(municipality, "MOCTEZUMA")>0
replace uniqueid=24023 if strpos(municipality, "RAYÓN")>0
replace uniqueid=24024 if strpos(municipality, "RIOVERDE")>0
replace uniqueid=24025 if strpos(municipality, "SALINAS")>0
replace uniqueid=24026 if strpos(municipality, "SAN ANTONIO")>0
replace uniqueid=24027 if strpos(municipality, "SAN CIRO DE ACOSTA")>0
replace uniqueid=24028 if strpos(municipality, "SAN LUIS POTOSÍ")>0
replace uniqueid=24029 if strpos(municipality, "SAN MARTÍN CHALCHICUAUTLA")>0
replace uniqueid=24030 if strpos(municipality, "SAN NICOLÁS TOLENTINO")>0
replace uniqueid=24034 if strpos(municipality, "SAN VICENTE TANCUAYALAB")>0
replace uniqueid=24031 if strpos(municipality, "SANTA CATARINA")>0
replace uniqueid=24032 if strpos(municipality, "SANTA MARÍA DEL RÍO")>0
replace uniqueid=24033 if strpos(municipality, "SANTO DOMINGO")>0
replace uniqueid=24035 if strpos(municipality, "SOLEDAD DE GRACIANO SÁNCHEZ")>0
replace uniqueid=24036 if strpos(municipality, "TAMASOPO")>0
replace uniqueid=24037 if strpos(municipality, "TAMAZUNCHALE")>0
replace uniqueid=24038 if strpos(municipality, "TAMPACÁN")>0
replace uniqueid=24039 if strpos(municipality, "TAMPAMOLÓN CORONA")>0
replace uniqueid=24040 if strpos(municipality, "TAMUÍN")>0
replace uniqueid=24012 if strpos(municipality, "TANCANHUITZ")>0
replace uniqueid=24041 if strpos(municipality, "TANLAJÁS")>0
replace uniqueid=24042 if strpos(municipality, "TANQUIÁN DE ESCOBEDO")>0
replace uniqueid=24043 if strpos(municipality, "TIERRA NUEVA")>0
replace uniqueid=24044 if strpos(municipality, "VANEGAS")>0
replace uniqueid=24045 if strpos(municipality, "VENADO")>0
replace uniqueid=24056 if strpos(municipality, "VILLA DE ARISTA")>0
replace uniqueid=24046 if strpos(municipality, "VILLA DE ARRIAGA")>0
replace uniqueid=24047 if strpos(municipality, "VILLA DE GUADALUPE")>0
replace uniqueid=24048 if strpos(municipality, "VILLA DE LA PAZ")>0
replace uniqueid=24049 if strpos(municipality, "VILLA DE RAMOS")>0
replace uniqueid=24050 if strpos(municipality, "VILLA DE REYES")>0
replace uniqueid=24051 if strpos(municipality, "VILLA HIDALGO")>0
replace uniqueid=24052 if strpos(municipality, "VILLA JUÁREZ")>0
replace uniqueid=24054 if strpos(municipality, "XILITLA")>0
replace uniqueid=24055 if strpos(municipality, "ZARAGOZA")>0

egen total = rowtotal(PAN_MC-PRI_PCP)

order CI_1, a(PRI_PCP)

drop CNR VTN

collapse (sum) PAN_MC-CI_1 total, by (municipality uniqueid SECCION)

rename SECCION section

egen valid = rowtotal(PAN_MC PRI PRD PT PVEM PCP PANAL MORENA PT_MORENA_PES PAN PRI_PVEM_PCP MC PAN_PRD_MC PRI_PVEM_PANAL PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP CI_1)

foreach var in PAN_MC PRI PRD PT PVEM PCP PANAL MORENA PT_MORENA_PES PAN PRI_PVEM_PCP MC PAN_PRD_MC PRI_PVEM_PANAL PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP CI_1 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN_MC i_PRI i_PRD i_PT i_PVEM i_PCP i_PANAL i_MORENA i_PT_MORENA_PES i_PAN i_PRI_PVEM_PCP i_MC i_PAN_PRD_MC i_PRI_PVEM_PANAL i_CI_1 i_PVEM_PANAL i_PRI_PVEM i_PRI_PANAL i_PRI_PANAL_PCP i_PRI_PVEM_PANAL_PCP i_PRI_PCP, ///
	gen(PAN_MC_r PRI_r PRD_r PT_r PVEM_r PCP_r PANAL_r MORENA_r PT_MORENA_PES_r PAN_r PRI_PVEM_PCP_r MC_r PAN_PRD_MC_r PRI_PVEM_PANAL_r CI_1_r PVEM_PANAL_r PRI_PVEM_r PRI_PANAL_r PRI_PANAL_PCP_r PRI_PVEM_PANAL_PCP_r PRI_PCP_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_MC PRI PRD PT PVEM PCP PANAL MORENA PT_MORENA_PES PAN PRI_PVEM_PCP MC PAN_PRD_MC PRI_PVEM_PANAL PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta

gen incumbent_vote = .

egen maxpan = rowmax(PAN PAN_MC PAN_PRD_MC)
replace incumbent_vote = maxpan if incumbent=="PAN"

egen maxprd = rowmax(PRD PAN_PRD_MC)
replace incumbent_vote = maxprd if incumbent=="PRD"

egen maxpri = rowmax(PRI PRI_PVEM_PCP PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP)
replace incumbent_vote = maxpri if incumbent=="PRI"

egen maxmorena = rowmax(MORENA PT_MORENA_PES)
replace incumbent_vote = maxmorena if incumbent=="MORENA"

egen maxpanal = rowmax(PANAL PRI_PVEM_PANAL PVEM_PANAL PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP)
replace incumbent_vote = maxpanal if incumbent=="PANAL"

egen maxpvem = rowmax(PVEM PRI_PVEM_PCP PRI_PVEM_PANAL PVEM_PANAL PRI_PVEM PRI_PVEM_PANAL_PCP)
replace incumbent_vote = maxpvem if incumbent=="PVEM"

* Assign vote shares to major parties from coalitions
egen maxpanpt = rowmax(PAN PAN_MC PAN_PRD_MC PT)
replace incumbent_vote = maxpanpt if incumbent=="PAN_PT"

egen maxpanptmc = rowmax(PT PAN_MC PT PAN MC PAN_PRD_MC)
replace incumbent_vote = maxprd if incumbent=="PAN_PT_MC"

egen maxprdmc = rowmax(PRD MC PAN_PRD_MC)
replace incumbent_vote = maxprdmc if incumbent=="PRD_MC"

egen maxptmc = rowmax(PT MC)
replace incumbent_vote = maxptmc if incumbent=="PT_MC"

egen maxprdpcp = rowmax(PRD PCP PAN_PRD_MC)
replace incumbent_vote = maxprdpcp if incumbent=="PRD_PCP"

egen maxprdpt = rowmax(PRD PT PAN_PRD_MC)
replace incumbent_vote = maxprdpt if incumbent=="PRD_PT"

egen maxprdptmcpanal = rowmax(PRD PT MC PANAL PAN_PRD_MC)
replace incumbent_vote = maxprdptmcpanal if incumbent=="PRD_PT_MC_PANAL"

egen maxprdptpanal = rowmax(PRD PT PANAL PAN_PRD_MC)
replace incumbent_vote = maxprdptpanal if incumbent=="PRD_PT_PANAL"

egen maxprdptmcpcp = rowmax(PRD PT MC PCP PAN_PRD_MC)
replace incumbent_vote = maxprdptmcpcp if incumbent=="PRD_PT_PCP_MC"

egen maxpripanal = rowmax(PRI PANAL PRI_PVEM_PCP PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP)
replace incumbent_vote = maxpripanal if incumbent=="PRI_PANAL"

egen maxpripvem = rowmax(PRI PRI_PVEM_PCP PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP)
replace incumbent_vote = maxpripvem if incumbent=="PRI_PVEM"

egen maxpripvempanal = rowmax(PRI PVEM PANAL PRI_PVEM_PCP PRI_PVEM_PANAL PRI_PVEM PRI_PANAL PRI_PANAL_PCP PRI_PVEM_PANAL_PCP PRI_PCP)
replace incumbent_vote = maxpripvempanal if incumbent=="PRI_PVEM_PANAL"

drop max*

* Specific codings by Salvador
replace incumbent_vote=PAN  if uniqueid==24002
replace incumbent_vote=PAN_MC  if uniqueid==24006 | uniqueid==24008
replace incumbent_vote=PAN_PRD_MC if uniqueid==24005
replace incumbent_vote=PAN if uniqueid==24054
replace incumbent_vote=PAN_MC if uniqueid==24021 | uniqueid==24055 | uniqueid==24056
replace incumbent_vote=PCP if uniqueid==24041
replace incumbent_vote=PRI if uniqueid==24015 | uniqueid==24020
replace incumbent_vote=PRI_PVEM_PANAL if uniqueid==24019 
replace incumbent_vote=PRI_PVEM if uniqueid==24010 |  uniqueid==24036 | uniqueid==24040 | uniqueid==24042 |  uniqueid==24043 |  uniqueid==24046 | uniqueid==24049
replace incumbent_vote=PRI_PVEM_PCP if uniqueid==24003 | uniqueid==24012
replace incumbent_vote=PRI_PVEM_PANAL_PCP if uniqueid==24013 | uniqueid==24051
replace incumbent_vote=PRI_PANAL if uniqueid==24032 |  uniqueid==24047  |  uniqueid==24050

g year=2018
g month="July"
g STATE="SAN LUIS POTOSI"

preserve
use "..\Listas Nominales\ListadoNominalPREP2018.dta", clear
keep if STATE=="SAN LUIS POTOSI"
save SLP_LN18.dta, replace
restore

merge 1:1 STATE section using SLP_LN18.dta
drop _merge 
erase SLP_LN18.dta

rename ListadoNominalINE listanominal

gen turnout=total/listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 
gen mun_turnout =  mun_total/mun_listanominal

save "San_Luis_Potosi_Section_2018.dta", replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use San_Luis_Potosi_Section_2018.dta
collapse (sum) PAN-CI_1 valid total listanominal incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\San_Luis_Potosi_2018.dta", replace

**********************************************************************
**********************************************************************

erase "Ahualulco.dta"
erase "Alaquines.dta"
erase "Aquismon.dta"
erase "Armadillo.dta"
erase "Cardenas.dta"
erase "Catorce.dta"
erase "Cedral.dta"
erase "Cerritos.dta"
erase "Cerro de San Pedro.dta"
erase "Ciudad del Maiz.dta"
erase "Ciudad Fernandez.dta"
erase "Ciudad Valles.dta"
erase "Tancanhuitz.dta"
erase "Coxcatlan.dta"
erase "Charcas.dta"
erase "Ebano.dta"
erase "Guadalzacar.dta"
erase "Huhuetlan.dta"
erase "Lagunillas.dta"
erase "Matehuala.dta"
erase "Mexquitic.dta"
erase "Moctezuma.dta"
erase "Rayon.dta"
erase "Rioverde.dta"
erase "Salinas.dta"
erase "San Antonio.dta"
erase "San Ciro.dta"
erase "San Luis.dta"
erase "San Martin.dta"
erase "San Nicolas.dta"
erase "San Vicente.dta"
erase "Sta Catarina.dta"
erase "Sta Maria.dta"
erase "Sto Domingo.dta"
erase "Soledad G S.dta"
erase "Tamasopo.dta"
erase "Tamazunchale.dta"
erase "Tampacan.dta"
erase "Tampamolon.dta"
erase "Tamuin.dta"
erase "Tanlajas.dta"
erase "Tanquian.dta"
erase "Tierra Nueva.dta"
erase "Vanegas.dta"
erase "Venado.dta"
erase "Villa de Arista.dta"
erase "Villa de Arriaga.dta"
erase "Villa de Guadalupe.dta"
erase "Villa de la Paz.dta"
erase "Villa de Ramos.dta"
erase "Villa de Reyes.dta"
erase "Villa Hidalgo.dta"
erase "Villa Juarez.dta"
erase "Axtla.dta"
erase "Xilitla.dta"
erase "Zaragoza.dta"
erase "El Naranjo.dta"
erase "Matlapa.dta"

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

clear
append using San_Luis_Potosi_Section_2015.dta
append using San_Luis_Potosi_Section_2018.dta

erase San_Luis_Potosi_Section_2015.dta
erase San_Luis_Potosi_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 & (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC Independent {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}
tab winner_counter
count if winner_counter==0

save San_Luis_Potosi_Section_15_18.dta, replace

use "..\..\Precinct\San_Luis_Potosi_ALL.dta", clear
append using San_Luis_Potosi_Section_15_18.dta

erase San_Luis_Potosi_Section_15_18.dta

save San_Luis_Potosi_ALL_SALVADOR.dta, replace 
