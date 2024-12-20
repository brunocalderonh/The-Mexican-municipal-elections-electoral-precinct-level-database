////////////////////////////////////////////
////////MICHOACAN PRECINCT PANEL UPDATE//////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Michoacan"

* First identify coalitions
import excel "computos_municipales_2015.xlsx", sheet("Hoja1") cellrange(A3:BY115) firstrow clear
keep D AW-BT
foreach x of varlist AW-BT {
	replace `x' = "" if `x'=="-"
	destring `x', replace
	replace `x' = `x'!=.
}
rename (AW-BT) (PAN_PRI_PRD_PANAL_PH_PES PAN_PRD_PT_PANAL_PH PAN_PRD_PT_PANAL_PES PRD_PT_PANAL_PH PRD_PT_PANAL_PES PAN_PRI_PVEM PAN_PRD_PT PRD_PT_PANAL PRD_PT_PH PRD_PT_PES PRD_PANAL_PES PT_PANAL_PH PT_PES_PH PAN_PRD PAN_PT PAN_MC PAN_PH PRI_PVEM PRD_PT PRD_PANAL PRD_PES PT_MC PT_PH PT_PES)
rename * coal_*
rename coal_D MUNICIPIO
replace MUNICIPIO = subinstr(MUNICIPIO, "Á", "A", .)
replace MUNICIPIO = subinstr(MUNICIPIO, "É", "E", .)
replace MUNICIPIO = subinstr(MUNICIPIO, "Í", "I", .)
replace MUNICIPIO = subinstr(MUNICIPIO, "Ó", "O", .)
replace MUNICIPIO = subinstr(MUNICIPIO, "Ú", "U", .)
replace MUNICIPIO = "COALCOMAN DE VAZQUEZ PALLARES" if MUNICIPIO=="COALCOMAN"
replace MUNICIPIO = "TIQUICHEO DE NICOLAS ROMERO" if MUNICIPIO=="TIQUICHEO"
replace MUNICIPIO = "COJUMATLAN DE REGULES" if MUNICIPIO=="REGULES"
save "coalitions.dta", replace

****Preprocessed, see auxiliary

import excel using "Ayuntamientos_Mich_2015.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_Mich_2015.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if CVO=="TOTAL" | CVO==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "LAGUNILLAS.dta"
append using "ARTEAGA.dta"
append using "ZIRACUARETIRO.dta"
append using "SUSUPUATO.dta"
append using "TZINTZUNTZAN.dta"
append using "ERONGARICUARO.dta"
append using "TINGAMBATO.dta"
append using "TEPALCATEPEC.dta"
append using "JUAREZ.dta"
append using "Regules .dta"
append using "TUZANTLA.dta"
append using "CHURINTZIO.dta"
append using "CHURUMUCO.dta"
append using "Periban.dta"
append using "CHINICUILA.dta"
append using "JUNGAPEO.dta"
append using "Tanhuato.dta"
append using "CHUCANDIRO.dta"
append using "Yurecuaro.dta"
append using "Marcos Castellanos.dta"
append using "Tangamandapio.dta"
append using "TLAZAZALCA.dta"
append using "INDAPARAPEO.dta"
append using "CHARO.dta"
append using "ANGAMACUTIRO.dta"
append using "APORO.dta"
append using "Ixtlan.dta"
append using "QUERENDARO.dta"
append using "SAN LUCAS.dta"
append using "IRIMBO.dta"
append using "Copandaro.dta"
append using "La Huacana.dta"
append using "Huandacareo.dta"
append using "La Piedad.dta"
append using "LOS REYES.dta"
append using "Epitacio huerta.dta"
append using "TZITZIO.dta"
append using "BRISEÑAS.dta"
append using "Senguio.dta"
append using "HUIRAMBA.dta"
append using "Jiquilpan.dta"
append using "MORELIA.dta"
append using "Sahuayo.dta"
append using "Venustiano Carranza.dta"
append using "Vista Hermosa.dta"
append using "Tlalpujahua.dta"
append using "Pajacuaran.dta"
append using "Chavinda.dta"
append using "Jacona.dta"
append using "TANGANCICUARO.dta"
append using "COENEO.dta"
append using "Villamar.dta"
append using "BUENAVISTA.dta"
append using "Zamora.dta"
append using "HUANIQUEO.dta"
append using "Jimenez.dta"
append using "APATZINGAN.dta"
append using "PUREPERO.dta"
append using "Zacapu.dta"
append using "OBREGON.dta"
append using "CHARAPAN.dta"
append using "CUITZEO.dta"
append using "TARIMBARO.dta"
append using "ZINAPECUARO.dta"
append using "COTIJA.dta"
append using "TANCITARO.dta"
append using "TOCUMBO.dta"
append using "NUEVO URECHO.dta"
append using "ANGANGUEO.dta"
append using "OCAMPO.dta"
append using "NOCUPETARO.dta"
append using "PATZCUARO.dta"
append using "URUAPAN.dta"
append using "MUGICA.dta"
append using "NVO PARANGARICUTIRO.dta"
append using "AGUILILLA.dta"
append using "CARACUARO.dta"
append using "SALVADOR ESCALANTE.dta"
append using "ROMERO.dta"
append using "QUIROGA.dta"
append using "Morelos.dta"
append using "HIDALGO.dta"
append using "Ario.dta"
append using "Aquila.dta"
append using "Chilchota.dta"
append using "ACUITZIO.dta"
append using "ZITACUARO.dta"
append using "COAHUAYANA.dta"
append using "MADERO.dta"
append using "PARACHO.dta"
append using "TUXPAN.dta"
append using "TACAMBARO.dta"
append using "COALCOMAN.dta"
append using "Cardenas.dta"
append using "HUETAMO.dta"
append using "ECUANDUREO.dta"
append using "Nahuatzen.dta"
append using "Jose Sixto Verduzco.dta"
append using "Puruandiro.dta"
append using "MARAVATIO.dta"
append using "Paracuaro.dta"
append using "Tumbiscatio.dta"
append using "Numaran.dta"
append using "TARETAN.dta"
append using "TINGÜINDIN.dta"
append using "SANTA ANA MAYA.dta"
append using "Zinaparo.dta"
append using "PANINDICUARO.dta"
append using "CONTEPEC.dta"
append using "GABRIEL ZAMORA.dta"
append using "TURICATO.dta"

drop W R V T U Q
destring *, replace
drop if MUNICIPIO==""

merge m:1 MUNICIPIO using "coalitions.dta"
drop if _merge==2
drop _merge
erase "coalitions.dta"

rename MUNICIPIO municipality
rename SECCIÓN section
drop if PAN=="N/I"
destring PAN, replace

drop SUMADEVOTOSVALIDOS BOLETASENCASILLA VOTACIONEMITIDA VOTACIONTOTAL

replace PAN=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1  
replace PRI=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1  
replace PRD=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1  
replace PANAL=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1  
replace PH=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1  
replace PES=. if coal_PAN_PRI_PRD_PANAL_PH_PES==1    
drop coal_PAN_PRI_PRD_PANAL_PH_PES

replace PAN=. if coal_PAN_PRD_PT_PANAL_PH==1  
replace PRD=. if coal_PAN_PRD_PT_PANAL_PH==1  
replace PT=. if coal_PAN_PRD_PT_PANAL_PH==1  
replace PES=. if coal_PAN_PRD_PT_PANAL_PH==1  
replace PANAL=. if coal_PAN_PRD_PT_PANAL_PH==1
drop coal_PAN_PRD_PT_PANAL_PH

replace PAN=. if coal_PAN_PRD_PT_PANAL_PES==1  
replace PRD=. if coal_PAN_PRD_PT_PANAL_PES==1  
replace PT=. if coal_PAN_PRD_PT_PANAL_PES==1  
replace PES=. if coal_PAN_PRD_PT_PANAL_PES==1  
replace PANAL=. if coal_PAN_PRD_PT_PANAL_PES==1
drop coal_PAN_PRD_PT_PANAL_PES

replace PRD=. if coal_PRD_PT_PANAL_PH==1
replace PT=. if coal_PRD_PT_PANAL_PH==1
replace PH=. if coal_PRD_PT_PANAL_PH==1
replace PANAL=. if coal_PRD_PT_PANAL_PH==1
drop coal_PRD_PT_PANAL_PH

replace PRD=. if coal_PRD_PT_PANAL_PES==1
replace PANAL=. if coal_PRD_PT_PANAL_PES==1
replace PES=. if coal_PRD_PT_PANAL_PES==1
replace PT=. if coal_PRD_PT_PANAL_PES==1
drop coal_PRD_PT_PANAL_PES

replace PAN=. if coal_PAN_PRI_PVEM==1
replace PRI=. if coal_PAN_PRI_PVEM==1
replace PVEM=. if coal_PAN_PRI_PVEM==1
drop coal_PAN_PRI_PVEM

replace PAN=. if coal_PAN_PRD_PT==1
replace PRD=. if coal_PAN_PRD_PT==1
replace PT=. if coal_PAN_PRD_PT==1
drop coal_PAN_PRD_PT

replace PRD=. if coal_PRD_PT_PANAL==1
replace PT=. if coal_PRD_PT_PANAL==1
replace PANAL=. if coal_PRD_PT_PANAL==1
drop coal_PRD_PT_PANAL

replace PRD=. if coal_PRD_PT_PH==1
replace PT=. if coal_PRD_PT_PH==1
replace PH=. if coal_PRD_PT_PH==1
drop coal_PRD_PT_PH

replace PRD=. if coal_PRD_PT_PES ==1
replace PT=. if coal_PRD_PT_PES ==1
replace PES=. if coal_PRD_PT_PES ==1
drop coal_PRD_PT_PES

replace PRD=. if coal_PRD_PANAL_PES==1
replace PANAL=. if coal_PRD_PANAL_PES==1
replace PES=. if coal_PRD_PANAL_PES==1
drop coal_PRD_PANAL_PES

replace PT=. if coal_PT_PANAL_PH==1
replace PANAL=. if coal_PT_PANAL_PH==1
replace PH=. if coal_PT_PANAL_PH==1
drop coal_PT_PANAL_PH

replace PT=. if coal_PT_PES_PH==1
replace PES=. if coal_PT_PES_PH==1
replace PH=. if coal_PT_PES_PH==1
drop coal_PT_PES_PH

replace PAN=. if coal_PAN_PRD==1   
replace PRD=. if coal_PAN_PRD==1
drop coal_PAN_PRD

replace PAN=. if coal_PAN_PT==1
replace PT=. if coal_PAN_PT==1
drop coal_PAN_PT

replace PAN=. if coal_PAN_MC==1
replace MC=. if coal_PAN_MC==1
drop coal_PAN_MC

replace PAN=. if coal_PAN_PH==1
replace PH=. if coal_PAN_PH==1
drop coal_PAN_PH

replace PRI=. if coal_PRI_PVEM ==1
replace PVEM=. if coal_PRI_PVEM ==1
drop coal_PRI_PVEM

replace PRD=. if coal_PRD_PT==1
replace PT=. if coal_PRD_PT==1
drop coal_PRD_PT

replace PRD=. if coal_PRD_PANAL==1
replace PANAL=. if coal_PRD_PANAL==1
drop coal_PRD_PANAL

replace PRD=. if coal_PRD_PES==1
replace PES=. if coal_PRD_PES==1
drop coal_PRD_PES

replace PT=. if coal_PT_MC==1
replace MC=. if coal_PT_MC==1
drop coal_PT_MC

replace PT=. if coal_PT_PH==1
replace PH=. if coal_PT_PH==1
drop coal_PT_PH

replace PT=. if coal_PT_PES==1
replace PES=. if coal_PT_PES==1
drop coal_PT_PES

replace municipality = proper(municipality)

gen   uniqueid= 0
replace uniqueid=16001 if municipality =="Acuitzio"
replace uniqueid=16002 if municipality =="Aguililla"
replace uniqueid=16003 if municipality =="Alvaro Obregon"
replace uniqueid=16004 if municipality =="Angamacutiro"
replace uniqueid=16005 if municipality =="Angangueo"
replace uniqueid=16006 if municipality =="Apatzingan"
replace uniqueid=16007 if municipality =="Aporo"
replace uniqueid=16008 if municipality =="Aquila"
replace uniqueid=16009 if municipality =="Ario"
replace uniqueid=16010 if municipality =="Arteaga"
replace uniqueid=16011 if municipality =="Brisenas" | municipality=="BriseÑAs"
replace uniqueid=16012 if municipality =="Buenavista"
replace uniqueid=16013 if municipality =="Caracuaro"
replace uniqueid=16021 if municipality =="Charapan"
replace uniqueid=16022 if municipality =="Charo"
replace uniqueid=16023 if municipality =="Chavinda"
replace uniqueid=16024 if municipality =="Cheran"
replace uniqueid=16025 if municipality =="Chilchota"
replace uniqueid=16026 if municipality =="Chinicuila"
replace uniqueid=16027 if municipality =="Chucandiro"
replace uniqueid=16028 if municipality =="Churintzio"
replace uniqueid=16029 if municipality =="Churumuco"
replace uniqueid=16014 if municipality =="Coahuayana"
replace uniqueid=16015 if municipality =="Coalcoman" | municipality=="Coalcoman De Vazquez Pallares"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="Regules" | municipality=="Cojumatlan De Regules"
replace uniqueid=16017 if municipality =="Contepec"
replace uniqueid=16018 if municipality =="Copandaro"
replace uniqueid=16019 if municipality =="Cotija"
replace uniqueid=16020 if municipality =="Cuitzeo"
replace uniqueid=16030 if municipality =="Ecuandureo"
replace uniqueid=16031 if municipality =="Epitacio Huerta"
replace uniqueid=16032 if municipality =="Erongaricuaro"
replace uniqueid=16033 if municipality =="Gabriel Zamora"
replace uniqueid=16034 if municipality =="Hidalgo"
replace uniqueid=16036 if municipality =="Huandacareo"
replace uniqueid=16037 if municipality =="Huaniqueo"
replace uniqueid=16038 if municipality =="Huetamo"
replace uniqueid=16039 if municipality =="Huiramba"
replace uniqueid=16040 if municipality =="Indaparapeo"
replace uniqueid=16041 if municipality =="Irimbo"
replace uniqueid=16042 if municipality =="Ixtlan"
replace uniqueid=16043 if municipality =="Jacona"
replace uniqueid=16044 if municipality =="Jimenez"
replace uniqueid=16045 if municipality =="Jiquilpan"
replace uniqueid=16113 if municipality =="Jose Sixto Verduzco"
replace uniqueid=16046 if municipality =="Juarez"
replace uniqueid=16047 if municipality =="Jungapeo"
replace uniqueid=16035 if municipality =="La Huacana"
replace uniqueid=16069 if municipality =="La Piedad"
replace uniqueid=16048 if municipality =="Lagunillas"
replace uniqueid=16052 if municipality =="Lazaro Cardenas"
replace uniqueid=16075 if municipality =="Los Reyes"
replace uniqueid=16049 if municipality =="Madero"
replace uniqueid=16050 if municipality =="Maravatio"
replace uniqueid=16051 if municipality =="Marcos Castellanos"
replace uniqueid=16053 if municipality =="Morelia"
replace uniqueid=16054 if municipality =="Morelos"
replace uniqueid=16055 if municipality =="Mugica"
replace uniqueid=16056 if municipality =="Nahuatzen"
replace uniqueid=16057 if municipality =="Nocupetaro"
replace uniqueid=16058 if municipality =="Nuevo Parangaricutiro"
replace uniqueid=16059 if municipality =="Nuevo Urecho"
replace uniqueid=16060 if municipality =="Numaran"
replace uniqueid=16061 if municipality =="Ocampo"
replace uniqueid=16062 if municipality =="Pajacuaran"
replace uniqueid=16063 if municipality =="Panindicuaro"
replace uniqueid=16065 if municipality =="Paracho"
replace uniqueid=16064 if municipality =="Paracuaro"
replace uniqueid=16066 if municipality =="Patzcuaro"
replace uniqueid=16067 if municipality =="Penjamillo"
replace uniqueid=16068 if municipality =="Periban"
replace uniqueid=16070 if municipality =="Purepero"
replace uniqueid=16071 if municipality =="Puruandiro"
replace uniqueid=16072 if municipality =="Querendaro"
replace uniqueid=16073 if municipality =="Quiroga"
replace uniqueid=16076 if municipality =="Sahuayo"
replace uniqueid=16079 if municipality =="Salvador Escalante"
replace uniqueid=16077 if municipality =="San Lucas"
replace uniqueid=16078 if municipality =="Santa Ana Maya"
replace uniqueid=16080 if municipality =="Senguio"
replace uniqueid=16081 if municipality =="Susupuato"
replace uniqueid=16082 if municipality =="Tacambaro"
replace uniqueid=16083 if municipality =="Tancitaro"
replace uniqueid=16084 if municipality =="Tangamandapio"
replace uniqueid=16085 if municipality =="Tangancicuaro"
replace uniqueid=16086 if municipality =="Tanhuato"
replace uniqueid=16087 if municipality =="Taretan"
replace uniqueid=16088 if municipality =="Tarimbaro"
replace uniqueid=16089 if municipality =="Tepalcatepec"
replace uniqueid=16090 if municipality =="Tingambato"
replace uniqueid=16091 if municipality =="Tinguindin" | municipality=="TingÜIndin"
replace uniqueid=16092 if municipality =="Tiquicheo" | municipality=="Tiquicheo De Nicolas Romero"
replace uniqueid=16093 if municipality =="Tlalpujahua"
replace uniqueid=16094 if municipality =="Tlazazalca"
replace uniqueid=16095 if municipality =="Tocumbo"
replace uniqueid=16096 if municipality =="Tumbiscatio"
replace uniqueid=16097 if municipality =="Turicato"
replace uniqueid=16098 if municipality =="Tuxpan"
replace uniqueid=16099 if municipality =="Tuzantla"
replace uniqueid=16100 if municipality =="Tzintzuntzan"
replace uniqueid=16101 if municipality =="Tzitzio"
replace uniqueid=16102 if municipality =="Uruapan"
replace uniqueid=16103 if municipality =="Venustiano Carranza"
replace uniqueid=16104 if municipality =="Villamar"
replace uniqueid=16105 if municipality =="Vista Hermosa"
replace uniqueid=16106 if municipality =="Yurecuaro"
replace uniqueid=16107 if municipality =="Zacapu"
replace uniqueid=16108 if municipality =="Zamora"
replace uniqueid=16109 if municipality =="Zinaparo"
replace uniqueid=16110 if municipality =="Zinapecuaro"
replace uniqueid=16111 if municipality =="Ziracuaretiro"
replace uniqueid=16112 if municipality =="Zitacuaro"

replace municipality = upper(municipality)

drop SUMATOTALDEVOTOS VOTACIÓNEMITIDA

collapse (sum) PAN-PAN_PRD_PT_PANAL_PH, by (municipality uniqueid section)

replace CI=CI+CI_1
drop CI_1
rename CI CI_1
	 
*PRI_PVEM PRD_PT_PANAL PRD_PT_PANAL_PES PRD_PANAL PRD_PANAL_PES PT_PANAL_PH PAN_PH PAN_MC 
*PAN_PRD_PT PRD_PES PRD_PT_PES PT_PES_PH PRD_PT_PANAL_PH PRD_PT_PH PT_MC PT_PH PRD_PT PAN_PT 
*PAN_PRI_PRD_PANAL_PH_PES PAN_PRD PAN_PRD_PT_PANAL_PES PAN_PRI_PVEM PVEM_PES PRD_PVEM_PES

egen valid=rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PRI_PVEM PRD_PT_PANAL PRD_PT_PANAL_PES PRD_PANAL PRD_PANAL_PES PT_PANAL_PH PAN_PH PAN_MC PAN_PRD_PT PRD_PES PRD_PT_PES PT_PES_PH CI_1 ///
	PT_PES PRD_PT_PANAL_PH PRD_PT_PH PT_MC PT_PH PRD_PT PAN_PT PAN_PRI_PRD_PANAL_PH_PES PAN_PRD PAN_PRD_PT_PANAL_PES PAN_PRI_PVEM PAN_PRD_PT_PANAL_PH)

gen total = valid + VOTOSNULOS + NOREGISTRADOS

drop NOREGISTRADOS VOTOSNULOS

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PRI_PVEM PRD_PT_PANAL PRD_PT_PANAL_PES PRD_PANAL PRD_PANAL_PES PT_PANAL_PH PAN_PH PAN_MC PAN_PRD_PT PRD_PES PRD_PT_PES PT_PES_PH CI_1 ///
	PT_PES PRD_PT_PANAL_PH PRD_PT_PH PT_MC PT_PH PRD_PT PAN_PT PAN_PRI_PRD_PANAL_PH_PES PAN_PRD PAN_PRD_PT_PANAL_PES PAN_PRI_PVEM PAN_PRD_PT_PANAL_PH total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PH i_PES i_PRI_PVEM i_PRD_PT_PANAL i_PRD_PT_PANAL_PES i_PRD_PANAL i_PRD_PANAL_PES i_PT_PANAL_PH i_PAN_PH i_PAN_MC i_PAN_PRD_PT i_PRD_PES i_PRD_PT_PES i_PT_PES_PH i_CI_1 ///
	i_PT_PES i_PRD_PT_PANAL_PH i_PRD_PT_PH i_PT_MC i_PT_PH i_PRD_PT i_PAN_PT i_PAN_PRI_PRD_PANAL_PH_PES i_PAN_PRD i_PAN_PRD_PT_PANAL_PES i_PAN_PRI_PVEM i_PAN_PRD_PT_PANAL_PH, gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PH_r PES_r PRI_PVEM_r PRD_PT_PANAL_r PRD_PT_PANAL_PES_r PRD_PANAL_r PRD_PANAL_PES_r PT_PANAL_PH_r PAN_PH_r PAN_MC_r PAN_PRD_PT_r PRD_PES_r PRD_PT_PES_r PT_PES_PH_r CI_1_r ///
	PT_PES_r PRD_PT_PANAL_PH_r PRD_PT_PH_r PT_MC_r PT_PH_r PRD_PT_r PAN_PT_r PAN_PRI_PRD_PANAL_PH_PES_r PAN_PRD_r PAN_PRD_PT_PANAL_PES_r PAN_PRI_PVEM_r PAN_PRD_PT_PANAL_PH_r)
drop i_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PRI_PVEM PRD_PT_PANAL PRD_PT_PANAL_PES PRD_PANAL PRD_PANAL_PES PT_PANAL_PH PAN_PH PAN_MC PAN_PRD_PT PRD_PES PRD_PT_PES PT_PES_PH CI_1 ///
	PT_PES PRD_PT_PANAL_PH PRD_PT_PH PT_MC PT_PH PRD_PT PAN_PT PAN_PRI_PRD_PANAL_PH_PES PAN_PRD PAN_PRD_PT_PANAL_PES PAN_PRI_PVEM PAN_PRD_PT_PANAL_PH {
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
g STATE="MICHOACAN"

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election (June 30 2015) 
keep entidad municipio seccion lista file year month
keep if entidad==16 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_MICH.dta, replace
restore

merge 1:1 section using LN15_MICH.dta
drop if _merge==2
drop _merge
erase LN15_MICH.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

gen mun_turnout =  mun_total/mun_listanominal
gen turnout=total/listanominal

save Michoacan_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
replace winner = "PAN_PRD_PANAL" if uniqueid==16076
rename winner incumbent

save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Michoacan_Section_2015.dta
order STATE municipality uniqueid * 
collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
save "..\..\Update Municipal\Michoacan_2015.dta", replace

**********************************************************************
**********************************************************************

erase "LAGUNILLAS.dta"
erase "ARTEAGA.dta"
erase "ZIRACUARETIRO.dta"
erase "SUSUPUATO.dta"
erase "TZINTZUNTZAN.dta"
erase "ERONGARICUARO.dta"
erase "TINGAMBATO.dta"
erase "TEPALCATEPEC.dta"
erase "JUAREZ.dta"
erase "Regules .dta"
erase "TUZANTLA.dta"
erase "CHURINTZIO.dta"
erase "CHURUMUCO.dta"
erase "Periban.dta"
erase "CHINICUILA.dta"
erase "JUNGAPEO.dta"
erase "Tanhuato.dta"
erase "CHUCANDIRO.dta"
erase "Yurecuaro.dta"
erase "Marcos Castellanos.dta"
erase "Tangamandapio.dta"
erase "TLAZAZALCA.dta"
erase "INDAPARAPEO.dta"
erase "CHARO.dta"
erase "ANGAMACUTIRO.dta"
erase "APORO.dta"
erase "Ixtlan.dta"
erase "QUERENDARO.dta"
erase "SAN LUCAS.dta"
erase "IRIMBO.dta"
erase "Copandaro.dta"
erase "La Huacana.dta"
erase "Huandacareo.dta"
erase "La Piedad.dta"
erase "LOS REYES.dta"
erase "Epitacio huerta.dta"
erase "TZITZIO.dta"
erase "BRISEÑAS.dta"
erase "Senguio.dta"
erase "HUIRAMBA.dta"
erase "Jiquilpan.dta"
erase "MORELIA.dta"
erase "Sahuayo.dta"
erase "Venustiano Carranza.dta"
erase "Vista Hermosa.dta"
erase "Tlalpujahua.dta"
erase "Pajacuaran.dta"
erase "Chavinda.dta"
erase "Jacona.dta"
erase "TANGANCICUARO.dta"
erase "COENEO.dta"
erase "Villamar.dta"
erase "BUENAVISTA.dta"
erase "Zamora.dta"
erase "HUANIQUEO.dta"
erase "Jimenez.dta"
erase "APATZINGAN.dta"
erase "PUREPERO.dta"
erase "Zacapu.dta"
erase "OBREGON.dta"
erase "CHARAPAN.dta"
erase "CUITZEO.dta"
erase "TARIMBARO.dta"
erase "ZINAPECUARO.dta"
erase "COTIJA.dta"
erase "TANCITARO.dta"
erase "TOCUMBO.dta"
erase "NUEVO URECHO.dta"
erase "ANGANGUEO.dta"
erase "OCAMPO.dta"
erase "NOCUPETARO.dta"
erase "PATZCUARO.dta"
erase "URUAPAN.dta"
erase "MUGICA.dta"
erase "NVO PARANGARICUTIRO.dta"
erase "AGUILILLA.dta"
erase "CARACUARO.dta"
erase "SALVADOR ESCALANTE.dta"
erase "ROMERO.dta"
erase "QUIROGA.dta"
erase "Morelos.dta"
erase "HIDALGO.dta"
erase "Ario.dta"
erase "Aquila.dta"
erase "Chilchota.dta"
erase "ACUITZIO.dta"
erase "ZITACUARO.dta"
erase "COAHUAYANA.dta"
erase "MADERO.dta"
erase "PARACHO.dta"
erase "TUXPAN.dta"
erase "TACAMBARO.dta"
erase "COALCOMAN.dta"
erase "Cardenas.dta"
erase "HUETAMO.dta"
erase "ECUANDUREO.dta"
erase "Nahuatzen.dta"
erase "Jose Sixto Verduzco.dta"
erase "Puruandiro.dta"
erase "MARAVATIO.dta"
erase "Paracuaro.dta"
erase "Tumbiscatio.dta"
erase "Numaran.dta"
erase "TARETAN.dta"
erase "TINGÜINDIN.dta"
erase "SANTA ANA MAYA.dta"
erase "Zinaparo.dta"
erase "PANINDICUARO.dta"
erase "CONTEPEC.dta"
erase "GABRIEL ZAMORA.dta"
erase "TURICATO.dta"

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

import excel "SAHUAYO ELECCION EXTRAORDINARIA.xlsx", sheet("COMPUTO FINAL") cellrange(A9:AE101) firstrow clear

rename E section
rename G listanominal
rename AC total

g municipality = "SAHUAYO EXTRAORDINARIO"
g uniqueid = 16076

drop if total==. | total==0 

rename U PAN_PRD_PANAL
rename Z PRI_PT_PVEM
rename M MC
rename O MORENA
rename P PES

collapse (sum) listanominal PAN_PRD_PANAL PRI_PT_PVEM MC MORENA PES total, by (uniqueid municipality section)

gen turnout = total/listanominal

egen valid = rowtotal(PAN_PRD_PANAL PRI_PT_PVEM MC MORENA PES)

foreach var in PAN_PRD_PANAL PRI_PT_PVEM MC MORENA PES total listanominal valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PANAL inv_mun_PRI_PT_PVEM inv_mun_MC inv_mun_MORENA inv_mun_PES, gen(PAN_PRD_PANAL_r PRI_PT_PVEM_r MC_r MORENA_r PES_r)
drop inv_mun_*

gen winner = "PAN_PRD_PANAL" if PAN_PRD_PANAL_r ==1
replace winner = "PRI_PT_PVEM" if PRI_PT_PVEM_r==1
replace winner = "MC" if MC_r==1
replace winner = "MORENA" if MORENA_r==1
replace winner = "PES" if PES_r==1
drop *_r

gen year = 2015
gen month ="December"

save Michoacan_Section_2015_EXTRAORDINARIO.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

import excel using "Ayuntamientos_Mich_2018.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_Mich_2018.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if ID_MUNICIPIO==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "Hoja1 (2).dta"
append using "Hoja1 (3).dta"
append using "Hoja1 (4).dta"
append using "Hoja1 (5).dta"
append using "Hoja1 (6).dta"
append using "Hoja1 (7).dta"
append using "Hoja1 (8).dta"
append using "Hoja1 (9).dta"
append using "Hoja1 (10).dta"
append using "Hoja1 (11).dta"
append using "Hoja1 (12).dta"
append using "Hoja1 (13).dta"
append using "Hoja1 (14).dta"
append using "Hoja1 (15).dta"
append using "Hoja1 (16).dta"
append using "Hoja1 (17).dta"
append using "Hoja1 (18).dta"
append using "Hoja1 (19).dta"
append using "Hoja1 (20).dta"
append using "Hoja1 (21).dta"
append using "Hoja1 (22).dta"
append using "Hoja1 (23).dta"
append using "Hoja1 (24).dta"
append using "Hoja1 (25).dta"
append using "Hoja1 (26).dta"
append using "Hoja1 (27).dta"
append using "Hoja1 (28).dta"
append using "Hoja1 (29).dta"
append using "Hoja1 (30).dta"
append using "Hoja1 (31).dta"
append using "Hoja1 (32).dta"
append using "Hoja1 (33).dta"
append using "Hoja1 (34).dta"
append using "Hoja1 (35).dta"
append using "Hoja1 (36).dta"
append using "Hoja1 (37).dta"
append using "Hoja1 (38).dta"
append using "Hoja1 (39).dta"
append using "Hoja1 (40).dta"
append using "Hoja1 (41).dta"
append using "Hoja1 (42).dta"
append using "Hoja1 (43).dta"
append using "Hoja1 (44).dta"
append using "Hoja1 (45).dta"
append using "Hoja1 (46).dta"
append using "Hoja1 (47).dta"
append using "Hoja1 (48).dta"
append using "Hoja1 (49).dta"
append using "Hoja1 (50).dta"
append using "Hoja1 (51).dta"
append using "Hoja1 (52).dta"
append using "Hoja1 (53).dta"
append using "Hoja1 (54).dta"
append using "Hoja1 (55).dta"
append using "Hoja1 (56).dta"
append using "Hoja1 (57).dta"
append using "Hoja1 (58).dta"
append using "Hoja1 (59).dta"
append using "Hoja1 (60).dta"
append using "Hoja1 (61).dta"
append using "Hoja1 (62).dta"
append using "Hoja1 (63).dta"
append using "Hoja1 (64).dta"
append using "Hoja1 (65).dta"
append using "Hoja1 (66).dta"
append using "Hoja1 (67).dta"
append using "Hoja1 (68).dta"
append using "Hoja1 (69).dta"
append using "Hoja1 (70).dta"
append using "Hoja1 (71).dta"
append using "Hoja1 (72).dta"
append using "Hoja1 (73).dta"
append using "Hoja1 (74).dta"
append using "Hoja1 (75).dta"
append using "Hoja1 (76).dta"
append using "Hoja1 (77).dta"
append using "Hoja1 (78).dta"
append using "Hoja1 (79).dta"
append using "Hoja1 (80).dta"
append using "Hoja1 (81).dta"
append using "Hoja1 (82).dta"
append using "Hoja1 (83).dta"
append using "Hoja1 (84).dta"
append using "Hoja1 (85).dta"
append using "Hoja1 (86).dta"
append using "Hoja1 (87).dta"
append using "Hoja1 (88).dta"
append using "Hoja1 (89).dta"
append using "Hoja1 (90).dta"
append using "Hoja1 (91).dta"
append using "Hoja1 (92).dta"
append using "Hoja1 (93).dta"
append using "Hoja1 (94).dta"
append using "Hoja1 (95).dta"
append using "Hoja1 (96).dta"
append using "Hoja1 (97).dta"
append using "Hoja1 (98).dta"
append using "Hoja1 (99).dta"
append using "Hoja1 (100).dta"
append using "Hoja1 (101).dta"
append using "Hoja1 (102).dta"
append using "Hoja1 (103).dta"
append using "Hoja1 (104).dta"
append using "Hoja1 (105).dta"
append using "Hoja1 (106).dta"
append using "Hoja1 (107).dta"
append using "Hoja1 (108).dta"
append using "Hoja1 (109).dta"
append using "Hoja1 (110).dta"
append using "Hoja1 (111).dta"
append using "Hoja1 (112).dta"
append using "Hoja1 (113).dta"

g municipality = proper(MUNICIPIO)

gen   uniqueid= 0
replace uniqueid=16001 if municipality =="Acuitzio"
replace uniqueid=16002 if municipality =="Aguililla"
replace uniqueid=16003 if municipality =="ÁLvaro ObregÓN"
replace uniqueid=16004 if municipality =="Angamacutiro"
replace uniqueid=16005 if municipality =="Angangueo"
replace uniqueid=16006 if municipality =="ApatzingÁN"
replace uniqueid=16007 if municipality =="ÁPoro"
replace uniqueid=16008 if municipality =="Aquila"
replace uniqueid=16009 if municipality =="Ario"
replace uniqueid=16010 if municipality =="Arteaga"
replace uniqueid=16011 if municipality =="Brisenas" | municipality=="BriseÑAs"
replace uniqueid=16012 if municipality =="Buenavista"
replace uniqueid=16013 if municipality =="CarÁCuaro"
replace uniqueid=16021 if municipality =="Charapan"
replace uniqueid=16022 if municipality =="Charo"
replace uniqueid=16023 if municipality =="Chavinda"
replace uniqueid=16024 if municipality =="Cheran"
replace uniqueid=16025 if municipality =="Chilchota"
replace uniqueid=16026 if municipality =="Chinicuila"
replace uniqueid=16027 if municipality =="ChucÁNdiro"
replace uniqueid=16028 if municipality =="Churintzio"
replace uniqueid=16029 if municipality =="Churumuco"
replace uniqueid=16014 if municipality =="Coahuayana"
replace uniqueid=16015 if municipality =="CoalcomÁN De Vazquez Pallares"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="CojumatlÁN De RÉGules"
replace uniqueid=16017 if municipality =="Contepec"
replace uniqueid=16018 if municipality =="CopÁNdaro"
replace uniqueid=16019 if municipality =="Cotija"
replace uniqueid=16020 if municipality =="Cuitzeo"
replace uniqueid=16030 if municipality =="Ecuandureo"
replace uniqueid=16031 if municipality =="Epitacio Huerta"
replace uniqueid=16032 if municipality =="ErongarÍCuaro"
replace uniqueid=16033 if municipality =="Gabriel Zamora"
replace uniqueid=16034 if municipality =="Hidalgo"
replace uniqueid=16036 if municipality =="Huandacareo"
replace uniqueid=16037 if municipality =="HuanÍQueo"
replace uniqueid=16038 if municipality =="Huetamo"
replace uniqueid=16039 if municipality =="Huiramba"
replace uniqueid=16040 if municipality =="Indaparapeo"
replace uniqueid=16041 if municipality =="Irimbo"
replace uniqueid=16042 if municipality =="IxtlÁN"
replace uniqueid=16043 if municipality =="Jacona"
replace uniqueid=16044 if municipality =="JimÉNez"
replace uniqueid=16045 if municipality =="Jiquilpan"
replace uniqueid=16113 if municipality =="JosÉ Sixto Verduzco"
replace uniqueid=16046 if municipality =="JuÁRez"
replace uniqueid=16047 if municipality =="Jungapeo"
replace uniqueid=16035 if municipality =="La Huacana"
replace uniqueid=16069 if municipality =="La Piedad"
replace uniqueid=16048 if municipality =="Lagunillas"
replace uniqueid=16052 if municipality =="LÁZaro CÁRdenas"
replace uniqueid=16075 if municipality =="Los Reyes"
replace uniqueid=16049 if municipality =="Madero"
replace uniqueid=16050 if municipality =="MaravatÍO"
replace uniqueid=16051 if municipality =="Marcos Castellanos"
replace uniqueid=16053 if municipality =="Morelia"
replace uniqueid=16054 if municipality =="Morelos"
replace uniqueid=16055 if municipality =="MÚGica"
replace uniqueid=16056 if municipality =="NahuÁTzen"
replace uniqueid=16057 if municipality =="NocupÉTaro"
replace uniqueid=16058 if municipality =="Nuevo Parangaricutiro"
replace uniqueid=16059 if municipality =="Nuevo Urecho"
replace uniqueid=16060 if municipality =="NumarÁN"
replace uniqueid=16061 if municipality =="Ocampo"
replace uniqueid=16062 if municipality =="PajacuarÁN"
replace uniqueid=16063 if municipality =="PanindÍCuaro"
replace uniqueid=16065 if municipality =="Paracho"
replace uniqueid=16064 if municipality =="ParÁCuaro"
replace uniqueid=16066 if municipality =="PÁTzcuaro"
replace uniqueid=16067 if municipality =="Penjamillo"
replace uniqueid=16068 if municipality =="PeribÁN"
replace uniqueid=16070 if municipality =="PurÉPero"
replace uniqueid=16071 if municipality =="PuruÁNdiro"
replace uniqueid=16072 if municipality =="QuerÉNdaro"
replace uniqueid=16073 if municipality =="Quiroga"
replace uniqueid=16076 if municipality =="Sahuayo"
replace uniqueid=16079 if municipality =="Salvador Escalante"
replace uniqueid=16077 if municipality =="San Lucas"
replace uniqueid=16078 if municipality =="Santa Ana Maya"
replace uniqueid=16080 if municipality =="Senguio"
replace uniqueid=16081 if municipality =="Susupuato"
replace uniqueid=16082 if municipality =="TacÁMbaro"
replace uniqueid=16083 if municipality =="TancÍTaro"
replace uniqueid=16084 if municipality =="Tangamandapio"
replace uniqueid=16085 if municipality =="TangancÍCuaro"
replace uniqueid=16086 if municipality =="Tanhuato"
replace uniqueid=16087 if municipality =="Taretan"
replace uniqueid=16088 if municipality =="TarÍMbaro"
replace uniqueid=16089 if municipality =="Tepalcatepec"
replace uniqueid=16090 if municipality =="Tingambato"
replace uniqueid=16091 if municipality =="TingÜIndÍN"
replace uniqueid=16092 if municipality =="Tiquicheo" | municipality=="Tiquicheo De Nicolas Romero"
replace uniqueid=16093 if municipality =="Tlalpujahua"
replace uniqueid=16094 if municipality =="Tlazazalca"
replace uniqueid=16095 if municipality =="Tocumbo"
replace uniqueid=16096 if municipality =="Tumbiscatio"
replace uniqueid=16097 if municipality =="Turicato"
replace uniqueid=16098 if municipality =="Tuxpan"
replace uniqueid=16099 if municipality =="Tuzantla"
replace uniqueid=16100 if municipality =="Tzintzuntzan"
replace uniqueid=16101 if municipality =="Tzitzio"
replace uniqueid=16102 if municipality =="Uruapan"
replace uniqueid=16103 if municipality =="Venustiano Carranza"
replace uniqueid=16104 if municipality =="Villamar"
replace uniqueid=16105 if municipality =="Vista Hermosa"
replace uniqueid=16106 if municipality =="YurÉCuaro"
replace uniqueid=16107 if municipality =="Zacapu"
replace uniqueid=16108 if municipality =="Zamora"
replace uniqueid=16109 if municipality =="ZinÁParo"
replace uniqueid=16110 if municipality =="ZinapÉCuaro"
replace uniqueid=16111 if municipality =="Ziracuaretiro"
replace uniqueid=16112 if municipality =="ZitÁCuaro"

destring *, replace

g PAN_PRD_PMC = CO_PAN_PRD_PMC + CO_PAN_PRD + CO_PAN_PMC + CO_PRD_PMC + PAN + PMC + PRD if CO_PAN_PRD_PMC!=.
replace PAN=. if CO_PAN_PRD_PMC!=.
replace PMC=. if CO_PAN_PRD_PMC!=.
replace PRD=. if CO_PAN_PRD_PMC!=.
gen coalfrente = CO_PAN_PRD_PMC!=.
drop CO_PAN_PRD_PMC CO_PAN_PRD CO_PAN_PMC CO_PRD_PMC 

g PAN_PMC = CC_PAN_PMC + PAN + PMC if CC_PAN_PMC!=.
replace PAN=0 if CC_PAN_PMC!=.
replace PMC=0 if CC_PAN_PMC!=.
g coalpanmc = CC_PAN_PMC!=.
drop CC_PAN_PMC

g PAN_PRD = CC_PAN_PRD + PAN + PRD if CC_PAN_PRD!=.
replace PAN=0 if CC_PAN_PRD!=.
replace PRD=0 if CC_PAN_PRD!=.
g coalpanprd = CC_PAN_PRD!=.
drop CC_PAN_PRD

g PRD_PMC = CC_PRD_PMC + PRD + PMC if CC_PRD_PMC!=.
replace PRD=0 if CC_PRD_PMC!=.
replace PMC=0 if CC_PRD_PMC!=.
g coalprdmc = CC_PRD_PMC!=.
drop CC_PRD_PMC

g PRD_PVEM = CC_PRD_PVEM + PRD + PVEM if CC_PRD_PVEM!=.
replace PRD=0 if CC_PRD_PVEM!=.
replace PVEM=0 if CC_PRD_PVEM!=.
g coalprdpvem = CC_PRD_PVEM!=.
drop CC_PRD_PVEM 

g PT_MORENA = CO_PT_MORENA + PT + MORENA if  CO_PT_MORENA!=.
replace PT=0 if  CO_PT_MORENA!=.
replace MORENA=0 if  CO_PT_MORENA!=. 
g coalptmorena = CO_PT_MORENA!=.
drop CO_PT_MORENA 

rename SECCION section
rename PNA PANAL
rename *PMC *MC

order coal*, a(PT_MORENA)
order PAN_PRD_MC PAN_MC PAN_PRD PRD_MC PRD_PVEM PT_MORENA CI_*, a(PES)

collapse (sum) PAN-CI_2 NOREG NULO (first) coal*, by (municipality section uniqueid)

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PES PAN_PRD_MC PT_MORENA PAN_MC PRD_PVEM PAN_PRD PRD_MC CI_1 CI_2)

gen total=valid + NULO + NOREG
drop NOREG NULO

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES PAN_PRD_MC PT_MORENA PAN_MC PRD_PVEM PAN_PRD PRD_MC CI_1 CI_2 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PES i_PAN_PRD_MC i_PT_MORENA i_PAN_MC i_PRD_PVEM i_PAN_PRD i_PRD_MC i_CI_1 i_CI_2, ///
	gen( PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PES_r PAN_PRD_MC_r PT_MORENA_r PAN_MC_r PRD_PVEM_r PAN_PRD_r PRD_MC_r CI_1_r CI_2_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES PAN_PRD_MC PT_MORENA PAN_MC PRD_PVEM PAN_PRD PRD_MC CI_1 CI_2 {
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
g STATE="MICHOACAN"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta
replace incumbent = "PAN_PRD_PANAL" if municipality=="Sahuayo"

g incumbent_vote = .

*Megacoalition PAN_PRI_PRD_PANAL_PH_PES in Tacambaro in 2015, Presidente Municipal affiliated to the PRI
replace incumbent_vote=PRI if uniqueid==16082
* Megacoalition PAN_PRD_PT_PANAL_PES in Nahuatzen in 2015, Presidente Municipal affiliated to the PRD
replace incumbent_vote=PRD_PVEM if uniqueid==16056
*Incumbent independent mayor in Morelia
replace incumbent_vote=CI_1 if incumbent=="Independent"

replace incumbent_vote=PAN_PRD_MC if (incumbent=="PAN" | strpos(incumbent, "PAN_")>0 | incumbent=="PRD" | incumbent=="MC" ) & coalfrente==1

replace incumbent_vote=PAN if incumbent=="PAN" & coalfrente==0
replace incumbent_vote=PAN_MC if incumbent=="PAN" & coalfrente==0 & coalpanmc==1
replace incumbent_vote=PAN_PRD if incumbent=="PAN" & coalfrente==0 & coalpanprd==1

replace incumbent_vote=PRD if incumbent=="PRD" & coalfrente==0 & coalprdpvem==0 & coalprdmc==0
replace incumbent_vote=PRD_PVEM if incumbent=="PRD" & coalfrente==0 & coalprdpvem==1
replace incumbent_vote=PRD_MC if incumbent=="PRD" & coalfrente==0 & coalprdmc==1
replace incumbent_vote=PAN_PRD if incumbent=="PRD" & coalfrente==0 & coalpanprd==1

replace incumbent_vote=MC if incumbent=="MC" & coalfrente==0 & coalprdmc==1 & coalpanmc==1
replace incumbent_vote=PAN_MC if incumbent=="MC" & coalpanmc==1
replace incumbent_vote=PRD_MC if incumbent=="MC" & coalprdmc==1

replace incumbent_vote=PRI if incumbent=="PRI"

replace incumbent_vote=PVEM if incumbent=="PVEM" & coalprdpvem==0
replace incumbent_vote=PRD_PVEM if incumbent=="PVEM" & coalprdpvem==1

replace incumbent_vote=PT if incumbent=="PT"

replace incumbent_vote=PT_MORENA if coalptmorena==1 & (incumbent=="MORENA" | incumbent=="PT" )

replace incumbent_vote=PANAL if incumbent=="PANAL"

egen maxpanprdpanal=rowmax(PAN PAN_PRD_MC PAN_MC PAN_PRD PRD_MC PRD_PVEM PRD PANAL)
replace incumbent_vote=maxpanprdpanal if incumbent=="PAN_PRD_PANAL"

egen maxpanprdptpanalph=rowmax(PAN PAN_PRD_MC PAN_MC PAN_PRD PRD_MC PRD_PVEM PRD PANAL PT_MORENA PT)
replace incumbent_vote=maxpanprdptpanalph if incumbent=="PAN_PRD_PT_PANAL_PH"

egen maxprdpanal=rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PANAL)
replace incumbent_vote=maxprdpanal if incumbent=="PRD_PANAL"

egen maxprdpes=rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PES)
replace incumbent_vote=maxprdpes if incumbent=="PRD_PES"

egen maxprdpanalpes=rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PANAL PES)
replace incumbent_vote=maxprdpanalpes if incumbent=="PRD_PANAL_PES"

egen maxprdpt =rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PT)
replace incumbent_vote=maxprdpt if incumbent=="PRD_PT"

egen maxprdptpanal =rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PT PANAL)
replace incumbent_vote=maxprdptpanal if incumbent=="PRD_PT_PANAL"

egen maxprdptpes =rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PT PES)
replace incumbent_vote=maxprdptpes if incumbent=="PRD_PT_PES"

egen maxprdptph =rowmax (PRD PAN_PRD_MC PAN_PRD PRD_MC PRD_PVEM PT)
replace incumbent_vote=maxprdptph if incumbent=="PRD_PT_PH"

egen maxpripvem=rowmax(PRI PVEM)
replace incumbent_vote=maxpripvem if incumbent=="PRI_PVEM"

egen maxptpes =rowmax (PT PT_MORENA PES)
replace incumbent_vote=maxptpes if incumbent=="PT_PES"

drop maxp*
drop coal*

preserve
use "..\Listas Nominales\ListadoNominalPREP2018.dta", clear
keep if STATE=="MICHOACAN"
save MICH_LN18.dta, replace
restore

merge 1:1 STATE section using MICH_LN18.dta
drop _merge 
erase MICH_LN18.dta

rename ListadoNominalINE listanominal

gen turnout=total/listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 
gen mun_turnout =  mun_total/mun_listanominal

save "Michoacan_Section_2018.dta", replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use "Michoacan_Section_2018.dta"
collapse (sum) PAN-total listanominal incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Michoacan_2018.dta", replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

clear
append using Michoacan_Section_2018.dta
append using Michoacan_Section_2015.dta
append using Michoacan_Section_2015_EXTRAORDINARIO.dta

erase Michoacan_Section_2018.dta
erase Michoacan_Section_2015.dta
erase Michoacan_Section_2015_EXTRAORDINARIO.dta

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

save Michoacan_Section_15_18.dta, replace

clear
use "..\..\Precinct\Michoacan_ALL.dta"
append using Michoacan_Section_15_18.dta
save Michoacan_ALL_SALVADOR.dta, replace 

erase Michoacan_Section_15_18.dta

erase "Hoja1 (2).dta"
erase "Hoja1 (3).dta"
erase "Hoja1 (4).dta"
erase "Hoja1 (5).dta"
erase "Hoja1 (6).dta"
erase "Hoja1 (7).dta"
erase "Hoja1 (8).dta"
erase "Hoja1 (9).dta"
erase "Hoja1 (10).dta"
erase "Hoja1 (11).dta"
erase "Hoja1 (12).dta"
erase "Hoja1 (13).dta"
erase "Hoja1 (14).dta"
erase "Hoja1 (15).dta"
erase "Hoja1 (16).dta"
erase "Hoja1 (17).dta"
erase "Hoja1 (18).dta"
erase "Hoja1 (19).dta"
erase "Hoja1 (20).dta"
erase "Hoja1 (21).dta"
erase "Hoja1 (22).dta"
erase "Hoja1 (23).dta"
erase "Hoja1 (24).dta"
erase "Hoja1 (25).dta"
erase "Hoja1 (26).dta"
erase "Hoja1 (27).dta"
erase "Hoja1 (28).dta"
erase "Hoja1 (29).dta"
erase "Hoja1 (30).dta"
erase "Hoja1 (31).dta"
erase "Hoja1 (32).dta"
erase "Hoja1 (33).dta"
erase "Hoja1 (34).dta"
erase "Hoja1 (35).dta"
erase "Hoja1 (36).dta"
erase "Hoja1 (37).dta"
erase "Hoja1 (38).dta"
erase "Hoja1 (39).dta"
erase "Hoja1 (40).dta"
erase "Hoja1 (41).dta"
erase "Hoja1 (42).dta"
erase "Hoja1 (43).dta"
erase "Hoja1 (44).dta"
erase "Hoja1 (45).dta"
erase "Hoja1 (46).dta"
erase "Hoja1 (47).dta"
erase "Hoja1 (48).dta"
erase "Hoja1 (49).dta"
erase "Hoja1 (50).dta"
erase "Hoja1 (51).dta"
erase "Hoja1 (52).dta"
erase "Hoja1 (53).dta"
erase "Hoja1 (54).dta"
erase "Hoja1 (55).dta"
erase "Hoja1 (56).dta"
erase "Hoja1 (57).dta"
erase "Hoja1 (58).dta"
erase "Hoja1 (59).dta"
erase "Hoja1 (60).dta"
erase "Hoja1 (61).dta"
erase "Hoja1 (62).dta"
erase "Hoja1 (63).dta"
erase "Hoja1 (64).dta"
erase "Hoja1 (65).dta"
erase "Hoja1 (66).dta"
erase "Hoja1 (67).dta"
erase "Hoja1 (68).dta"
erase "Hoja1 (69).dta"
erase "Hoja1 (70).dta"
erase "Hoja1 (71).dta"
erase "Hoja1 (72).dta"
erase "Hoja1 (73).dta"
erase "Hoja1 (74).dta"
erase "Hoja1 (75).dta"
erase "Hoja1 (76).dta"
erase "Hoja1 (77).dta"
erase "Hoja1 (78).dta"
erase "Hoja1 (79).dta"
erase "Hoja1 (80).dta"
erase "Hoja1 (81).dta"
erase "Hoja1 (82).dta"
erase "Hoja1 (83).dta"
erase "Hoja1 (84).dta"
erase "Hoja1 (85).dta"
erase "Hoja1 (86).dta"
erase "Hoja1 (87).dta"
erase "Hoja1 (88).dta"
erase "Hoja1 (89).dta"
erase "Hoja1 (90).dta"
erase "Hoja1 (91).dta"
erase "Hoja1 (92).dta"
erase "Hoja1 (93).dta"
erase "Hoja1 (94).dta"
erase "Hoja1 (95).dta"
erase "Hoja1 (96).dta"
erase "Hoja1 (97).dta"
erase "Hoja1 (98).dta"
erase "Hoja1 (99).dta"
erase "Hoja1 (100).dta"
erase "Hoja1 (101).dta"
erase "Hoja1 (102).dta"
erase "Hoja1 (103).dta"
erase "Hoja1 (104).dta"
erase "Hoja1 (105).dta"
erase "Hoja1 (106).dta"
erase "Hoja1 (107).dta"
erase "Hoja1 (108).dta"
erase "Hoja1 (109).dta"
erase "Hoja1 (110).dta"
erase "Hoja1 (111).dta"
erase "Hoja1 (112).dta"
erase "Hoja1 (113).dta"
