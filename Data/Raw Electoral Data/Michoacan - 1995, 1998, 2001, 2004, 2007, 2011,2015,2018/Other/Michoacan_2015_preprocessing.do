////////////////////////////////////////////
////////MICHOACAN 2015//////


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

save Michoacan_Section_2015.csv, replace


