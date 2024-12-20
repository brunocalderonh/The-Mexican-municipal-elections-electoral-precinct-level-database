////////////////////////////////////////////
////////HIDALGO PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Hidalgo"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel using "Ayuntamientos_Hgo_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_Hgo_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	g municipio="`sheetname'"
	foreach x of varlist _all {
		cap replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "Acatlán_A.dta"
append using "Acaxochitlán_A.dta"
append using "Actopan_A.dta"
append using "Agua Blanca de Iturbide_A.dta"
append using "Ajacuba_A.dta"
append using "Alfajayucan_A.dta"
append using "Almoloya_A.dta"
append using "Apan_A.dta"
append using "Atitalaquia_A.dta"
append using "Atlapexco_A.dta"
append using "Atotonilco de Tula_A.dta"
append using "Atotonilco el Grande_A.dta"
append using "Calnali_A.dta"
append using "Cardonal_A.dta"
append using "Chapantongo_A.dta"
append using "Chapulhuacan_A.dta"
append using "Chilcuahutla_A.dta"
append using "Cuahutepec de Hinojosa_A.dta"
append using "El Arenal_A.dta"
append using "Eloxochitlán_A.dta"
append using "Emiliano Zapata_A.dta"
append using "Epazoyucan_A.dta"
append using "Francisco I Madero_A.dta"
append using "Huautla_A.dta"
append using "Huasca De Ocampo_A.dta"
append using "Huazalingo_A.dta"
append using "Huehuetla_A.dta"
append using "Huejutla De Reyes_A.dta"
append using "Huichapan_A.dta"
append using "Ixmiquilpan_A.dta"
append using "Jacala de Ledezma_A.dta"
append using "Jaltocan_A.dta"
append using "Juárez Hidalgo_A.dta"
append using "La Misión_A.dta"
append using "Lolotla_A.dta"
append using "Metepec_A.dta"
append using "Metztitlán_A.dta"
append using "Mineral De La Reforma_A.dta"
append using "Mineral Del Chico_A.dta"
append using "Mineral Del Monte_A.dta"
append using "Mixquiahuala De Juárez_A.dta"
append using "Molango De Escamilla_A.dta"
append using "Nicolás Flores_A.dta"
append using "Nopala De Villagrán_A.dta"
append using "Omitlán De Juárez_A.dta"
append using "Pachuca de Soto_A.dta"
append using "Pacula_A.dta"
append using "Santiago de Anaya_A.dta"
append using "Pisaflores_A.dta"
append using "Progreso De Obregón_A.dta"
append using "San Agustín Metzquititlán_A.dta"
append using "San Agustín Tlaxiaca_A.dta"
append using "San Bartolo Tutotepec_A.dta"
append using "San Felipe Orizatlán_A.dta"
append using "San Salvador_A.dta"
append using "Santiago Tulantepec_A.dta"
append using "Singuilucan_A.dta"
append using "Tasquillo_A.dta"
append using "Tecozautla_A.dta"
append using "Tenango de Doria_A.dta"
append using "Tepeapulco_A.dta"
append using "Tepehuacan de Guerrero_A.dta"
append using "Tepeji Del Río De Ocampo_A.dta"
append using "Tepetitlán_A.dta"
append using "Tetepango_A.dta"
append using "Tezontepec De Aldama_A.dta"
append using "Tianguistengo_A.dta"
append using "Tizayuca_A.dta"
append using "Tlahuelilpan_A.dta"
append using "Tlahuiltepa_A.dta"
append using "Tlanalapa_A.dta"
append using "Tlanchinol_A.dta"
append using "Tlaxcoapan_A.dta"
append using "Tolcayuca_A.dta"
append using "Tula De Allende_A.dta"
append using "Tulancingo De Bravo_A.dta"
append using "Villa De Tezontepec_A.dta"
append using "Xochiatipan_A.dta"
append using "Xochicoatlán_A.dta"
append using "Yahualica_A.dta"
append using "Zacualtipán De Ángeles_A.dta"
append using "Zapotlán De Juárez_A.dta"
append using "Zempoala_A.dta"
append using "Zimapán_A.dta"

drop Q U V W X Y Z AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ BA BB BC BD O S P R

replace Municipio="Zapotlán De Juárez" if Municipio=="Zapotlán De Juárez1619" | Municipio=="Zapotlán De Juárez1620" 
replace Municipio="Tula De Allende" if Municipio=="Tula De Allende " | Municipio=="Tula De Allende  "  | Municipio=="Tula De Allende   "

g municipality = upper(Municipio)

gen uniqueid = .
replace uniqueid=13001 if municipality =="ACATLáN"
replace uniqueid=13002 if municipality =="ACAXOCHITLáN"
replace uniqueid=13003 if municipality =="ACTOPAN"
replace uniqueid=13004 if municipality =="AGUA BLANCA DE ITURBIDE"
replace uniqueid=13005 if municipality =="AJACUBA"
replace uniqueid=13006 if municipality =="ALFAJAYUCAN"
replace uniqueid=13007 if municipality =="ALMOLOYA"
replace uniqueid=13008 if municipality =="APAN"
replace uniqueid=13010 if municipality =="ATITALAQUIA"
replace uniqueid=13011 if municipality =="ATLAPEXCO"
replace uniqueid=13013 if municipality =="ATOTONILCO DE TULA"
replace uniqueid=13012 if municipality =="ATOTONILCO EL GRANDE"
replace uniqueid=13014 if municipality =="CALNALI"
replace uniqueid=13015 if municipality =="CARDONAL"
replace uniqueid=13017 if municipality =="CHAPANTONGO"
replace uniqueid=13018 if municipality =="CHAPULHUACáN"
replace uniqueid=13019 if municipality =="CHILCUAUTLA"
replace uniqueid=13016 if municipality =="CUAUTEPEC DE HINOJOSA"
replace uniqueid=13009 if municipality =="EL ARENAL"
replace uniqueid=13020 if municipality =="ELOXOCHITLáN"
replace uniqueid=13021 if municipality =="EMILIANO ZAPATA"
replace uniqueid=13022 if municipality =="EPAZOYUCAN"
replace uniqueid=13023 if municipality =="FRANCISCO I. MADERO"
replace uniqueid=13024 if municipality =="HUASCA DE OCAMPO"
replace uniqueid=13025 if municipality =="HUAUTLA"
replace uniqueid=13026 if municipality =="HUAZALINGO"
replace uniqueid=13027 if municipality =="HUEHUETLA"
replace uniqueid=13028 if municipality =="HUEJUTLA DE REYES"
replace uniqueid=13029 if municipality =="HUICHAPAN"
replace uniqueid=13030 if municipality =="IXMIQUILPAN"
replace uniqueid=13031 if municipality =="JACALA DE LEDEZMA"
replace uniqueid=13032 if municipality =="JALTOCAN"
replace uniqueid=13033 if municipality =="JUáREZ HIDALGO"
replace uniqueid=13040 if municipality =="LA MISIóN"
replace uniqueid=13034 if municipality =="LOLOTLA"
replace uniqueid=13035 if municipality =="METEPEC"
replace uniqueid=13037 if municipality =="METZTITLáN"
replace uniqueid=13051 if municipality =="MINERAL DE LA REFORMA"
replace uniqueid=13038 if municipality =="MINERAL DEL CHICO"
replace uniqueid=13039 if municipality =="MINERAL DEL MONTE"
replace uniqueid=13041 if municipality =="MIXQUIAHUALA DE JUáREZ"
replace uniqueid=13042 if municipality =="MOLANGO DE ESCAMILLA"
replace uniqueid=13043 if municipality =="NICOLAS DE FLORES"
replace uniqueid=13043 if municipality =="NICOLáS FLORES"
replace uniqueid=13044 if municipality =="NOPALA DE VILLAGRáN"
replace uniqueid=13045 if municipality =="OMITLáN DE JUáREZ"
replace uniqueid=13048 if municipality =="PACHUCA DE SOTO"
replace uniqueid=13047 if municipality =="PACULA"
replace uniqueid=13049 if municipality =="PISAFLORES"
replace uniqueid=13050 if municipality =="PROGRESO DE OBREGóN"
replace uniqueid=13036 if municipality =="SAN AGUSTíN METZQUITITLáN"
replace uniqueid=13052 if municipality =="SAN AGUSTíN TLAXIACA"
replace uniqueid=13053 if municipality =="SAN BARTOLO TUTOTEPEC"
replace uniqueid=13046 if municipality =="SAN FELIPE ORIZATLáN"
replace uniqueid=13054 if municipality =="SAN SALVADOR"
replace uniqueid=13055 if municipality =="SANTIAGO DE ANAYA"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC DE LUGO GUERRERO"
replace uniqueid=13056 if municipality =="SANTIAGO TULANTEPEC"
replace uniqueid=13057 if municipality =="SINGUILUCAN"
replace uniqueid=13058 if municipality =="TASQUILLO"
replace uniqueid=13059 if municipality =="TECOZAUTLA"
replace uniqueid=13060 if municipality =="TENANGO DE DORIA"
replace uniqueid=13061 if municipality =="TEPEAPULCO"
replace uniqueid=13062 if municipality =="TEPEHUACáN DE GUERRERO"
replace uniqueid=13063 if municipality =="TEPEJI DEL RíO DE OCAMPO"
replace uniqueid=13064 if municipality =="TEPETITLáN"
replace uniqueid=13065 if municipality =="TETEPANGO"
replace uniqueid=13067 if municipality =="TEZONTEPEC DE ALDAMA"
replace uniqueid=13068 if municipality =="TIANGUISTENGO"
replace uniqueid=13069 if municipality =="TIZAYUCA"
replace uniqueid=13070 if municipality =="TLAHUELILPAN"
replace uniqueid=13071 if municipality =="TLAHUILTEPA"
replace uniqueid=13072 if municipality =="TLANALAPA"
replace uniqueid=13073 if municipality =="TLANCHINOL"
replace uniqueid=13074 if municipality =="TLAXCOAPAN"
replace uniqueid=13075 if municipality =="TOLCAYUCA"
replace uniqueid=13076 if municipality =="TULA DE ALLENDE"
replace uniqueid=13077 if municipality =="TULANCINGO DE BRAVO"
replace uniqueid=13066 if municipality =="VILLA DE TEZONTEPEC"
replace uniqueid=13078 if municipality =="XOCHIATIPAN"
replace uniqueid=13079 if municipality =="XOCHICOATLáN"
replace uniqueid=13080 if municipality =="YAHUALICA"
replace uniqueid=13081 if municipality =="ZACUALTIPáN DE ÁNGELES"
replace uniqueid=13082 if municipality =="ZAPOTLáN DE JUáREZ"
replace uniqueid=13083 if municipality =="ZEMPOALA"
replace uniqueid=13084 if municipality =="ZIMAPáN"

drop if uniqueid==.
drop municipality

destring *,replace

rename ENRIQUEPACHECOLOPEZ CI_3
replace CI_3=0 if CI_3==.

foreach var in JGUADALUPESANTIAGOAGUILAR ANDRÉSGILDARDOOCADIZIBARRA JOSÉLUISARROYAVEHERNÁNDEZ LUCIACARIÑOVITEHERNANDEZ JUANLUISHERNÁNDEZCÁZARES MARIOJARILLOHERNÁNDEZ JAVIERGÓMEZPICHARDO {
	replace `var'=0 if `var'==.
}

gen CI_2=0
replace CI_2= JGUADALUPESANTIAGOAGUILAR + ANDRÉSGILDARDOOCADIZIBARRA + JOSÉLUISARROYAVEHERNÁNDEZ + LUCIACARIÑOVITEHERNANDEZ + JUANLUISHERNÁNDEZCÁZARES + MARIOJARILLOHERNÁNDEZ + JAVIERGÓMEZPICHARDO
drop JGUADALUPESANTIAGOAGUILAR ANDRÉSGILDARDOOCADIZIBARRA JOSÉLUISARROYAVEHERNÁNDEZ LUCIACARIÑOVITEHERNANDEZ JUANLUISHERNÁNDEZCÁZARES MARIOJARILLOHERNÁNDEZ JAVIERGÓMEZPICHARDO

replace PartidoRevolucionarioInstituci=0 if PartidoRevolucionarioInstituci==.
replace PartidoRevolucionarioInstitutc=0 if PartidoRevolucionarioInstitutc==.
gen PRI= PartidoRevolucionarioInstituci + PartidoRevolucionarioInstitutc
drop PartidoRevolucionarioInstituci PartidoRevolucionarioInstitutc

replace PartidoVerde=0 if PartidoVerde==.
replace PartidoVerdeEcologistadeMéxi =0 if PartidoVerdeEcologistadeMéxi==.
gen PVEM = PartidoVerde + PartidoVerdeEcologistadeMéxi 
drop PartidoVerde PartidoVerdeEcologistadeMéxi 

replace PartidodelaRevoluciónDemocrá=0 if PartidodelaRevoluciónDemocrá==.
replace PartidodelaRevolcuiónDemocrá=0 if PartidodelaRevolcuiónDemocrá==.
gen PRD= PartidodelaRevoluciónDemocrá + PartidodelaRevolcuiónDemocrá
drop PartidodelaRevoluciónDemocrá PartidodelaRevolcuiónDemocrá

rename PartidoAcciónNacional PAN
rename PartidodelTrabajo PT
rename PartidoMovimientoCiudadano MC
rename PartidoNuevaAlianza PANAL
rename PartidoMorena MORENA
rename PartidoEncuentroSocial PES
 
foreach var in LUCINAMARÍACONCEPCIÓNANTÚNEZS NÉSTORMEJÍANERI ARMANDOSALOMÓNCAMARGO ÁNGELCRISTIANSOLÍSSOTO HÉCTORHERRERAOCAMPO ADÁNRÍOSESTRADA RUFINOMONTIELESCUDERO {
	replace `var'=0 if `var'==.
}

egen CI_1a = rowtotal(LUCINAMARÍACONCEPCIÓNANTÚNEZS NÉSTORMEJÍANERI ARMANDOSALOMÓNCAMARGO ÁNGELCRISTIANSOLÍSSOTO HÉCTORHERRERAOCAMPO ADÁNRÍOSESTRADA RUFINOMONTIELESCUDERO)
drop LUCINAMARÍACONCEPCIÓNANTÚNEZS NÉSTORMEJÍANERI ARMANDOSALOMÓNCAMARGO ÁNGELCRISTIANSOLÍSSOTO HÉCTORHERRERAOCAMPO ADÁNRÍOSESTRADA RUFINOMONTIELESCUDERO

foreach var in JUANCARLOSSÁNCHEZRIVERA FERNANDOCERÓNSOSA VÍCTORAUGUSTOSÁNCHEZDELGADO HÉCTORGARCÍABRAVO JOSÉLUISMUÑOZSOTO FORTUNATOGONZÁLEZISLAS HUMBERTOENDONIOSALINAS NOLDYNTORRESMARTÍNEZ FRANCISCOMACIASBELTRAN {
	replace `var'=0 if `var'==.
}
egen CI_1b = rowtotal(JUANCARLOSSÁNCHEZRIVERA FERNANDOCERÓNSOSA VÍCTORAUGUSTOSÁNCHEZDELGADO HÉCTORGARCÍABRAVO JOSÉLUISMUÑOZSOTO FORTUNATOGONZÁLEZISLAS HUMBERTOENDONIOSALINAS NOLDYNTORRESMARTÍNEZ FRANCISCOMACIASBELTRAN)
drop JUANCARLOSSÁNCHEZRIVERA FERNANDOCERÓNSOSA VÍCTORAUGUSTOSÁNCHEZDELGADO HÉCTORGARCÍABRAVO JOSÉLUISMUÑOZSOTO FORTUNATOGONZÁLEZISLAS HUMBERTOENDONIOSALINAS NOLDYNTORRESMARTÍNEZ FRANCISCOMACIASBELTRAN

foreach var in MINERVADURANVIVAR JULIORAMÓNMENCHACASALAZAR HEBLENANGELESHERNÁNDEZ LISSETMARCELINOTOVAR MARCOANTONIOOLVERAJIMÉNEZ JAVIERTELLEZMENDOZA MARCOSGONZALEZTREJO RAÚLGONZÁLEZGARCÍA JESÚSORTIZCANO MARÍAELENAJIMÉNEZGUZMÁN FRANCISCOJAVIERHERNANDEZCORTE CRISOFORORUANOVITE ALEJANDROAHUEDSARQUIS{
	replace `var'=0 if `var'==.
}
egen CI_1c = rowtotal(MINERVADURANVIVAR JULIORAMÓNMENCHACASALAZAR HEBLENANGELESHERNÁNDEZ LISSETMARCELINOTOVAR MARCOANTONIOOLVERAJIMÉNEZ JAVIERTELLEZMENDOZA MARCOSGONZALEZTREJO RAÚLGONZÁLEZGARCÍA JESÚSORTIZCANO MARÍAELENAJIMÉNEZGUZMÁN FRANCISCOJAVIERHERNANDEZCORTE CRISOFORORUANOVITE ALEJANDROAHUEDSARQUIS)
drop MINERVADURANVIVAR JULIORAMÓNMENCHACASALAZAR HEBLENANGELESHERNÁNDEZ LISSETMARCELINOTOVAR MARCOANTONIOOLVERAJIMÉNEZ JAVIERTELLEZMENDOZA MARCOSGONZALEZTREJO RAÚLGONZÁLEZGARCÍA JESÚSORTIZCANO MARÍAELENAJIMÉNEZGUZMÁN FRANCISCOJAVIERHERNANDEZCORTE CRISOFORORUANOVITE ALEJANDROAHUEDSARQUIS

gen CI_1= CI_1a + CI_1b + CI_1c + CI
drop CI_1a CI_1b CI_1c CI

drop T

rename PRIVERDENUEVAALIANZA PRI_PVEM_PANAL
replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRIVERDE + PRINUEVAALIANZA + VERDENUEVAALIANZA + PRI + PVEM + PANAL if PRI_PVEM_PANAL!=. 
replace PRI=. if PRI_PVEM_PANAL!=. 
replace PVEM=. if PRI_PVEM_PANAL!=. 
replace PANAL=. if PRI_PVEM_PANAL!=. 
drop PRIVERDE PRINUEVAALIANZA VERDENUEVAALIANZA

gen no_reg=0
replace no_reg= NOREGISTRADO + CNR
drop NOREGISTRADO CNR
gen nulo=0
replace nulo= NULO + Nulos + VOTOSNULOS
drop NULO Nulos VOTOSNULOS

replace Sección = NOCASILLA if Sección==.
replace Sección = 659 if Sección==650 & Municipio=="Lolotla"
replace Casilla = TIPODECASILLA if Casilla==""

collapse (sum) PAN-PRI_PVEM_PANAL CI_3 CI_2-nulo, by (Sección uniqueid Municipio)

rename Municipio municipality
rename Sección section

egen valid=rowtotal(PAN PT MC PANAL MORENA PES PRI_PVEM_PANAL PRI PVEM PRD CI_1 CI_2 CI_3)

g total=valid+ nulo + no_reg

foreach var in PAN PT MC PANAL MORENA PES PRI_PVEM_PANAL PRI PVEM PRD CI_1 CI_2 CI_3 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PT i_MC i_PANAL i_MORENA i_PES i_PRI_PVEM_PANAL i_PRI i_PVEM i_PRD  /// 
         i_CI_1 i_CI_2 i_CI_3, gen(PAN_r PT_r MC_r PANAL_r MORENA_r PES_r PRI_PVEM_PANAL_r PRI_r PVEM_r PRD_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PT MC PANAL MORENA PES PRI_PVEM_PANAL PRI PVEM PRD CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

drop no_reg 

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"   | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2016
gen month="June"
gen STATE="HIDALGO"

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

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep entidad municipio seccion lista file year month
keep if entidad==13 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN16_HGO.dta, replace
restore

merge 1:1 section using LN16_HGO.dta
keep if _merge==3
drop _merge
erase LN16_HGO.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

gen mun_turnout =  mun_total/mun_listanominal
gen turnout=total/listanominal

save Hidalgo_Section_2016.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-nulo valid total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Hidalgo_2016.dta", replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import delimited "Ayu_Seccion_2016_Extraordinario.csv", clear

rename seccion section
rename municipio municipality
rename total_votos total
rename lista_nominal listanominal
rename pan PAN
rename pri PRI
rename prd PRD
rename pvem PVEM
rename pt PT
rename mc MC

drop if total==. | total==0

collapse (sum) PAN-MC total listanominal, by (municipality section)

g turnout = total/listanominal

g uniqueid = 13045

replace municipality = "Omitlán de Juárez EXTRAORDINARIO"

egen valid = rowtotal(PAN PRI PRD PVEM PT MC)

foreach var in PAN PRI PRD PVEM PT MC total listanominal valid{
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen inv_mun_`var'= 1/mun_`var'
}

g mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PT inv_mun_MC, gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PT" if PT_r==1
replace winner = "MC" if MC_r==1
drop *_r

gen year =2016
gen month ="December"

sort section

save Hidalgo_Section_2016_Extraordinario.dta, replace

*****************************************

clear 
append using "..\..\Precinct\Hidalgo_ALL.dta"
append using Hidalgo_Section_2016.dta
append using Hidalgo_Section_2016_Extraordinario.dta

erase Hidalgo_Section_2016.dta
erase Hidalgo_Section_2016_Extraordinario.dta

save Hidalgo_ALL_SALVADOR.dta, replace

erase "Acatlán_A.dta"
erase "Acaxochitlán_A.dta"
erase "Actopan_A.dta"
erase "Agua Blanca de Iturbide_A.dta"
erase "Ajacuba_A.dta"
erase "Alfajayucan_A.dta"
erase "Almoloya_A.dta"
erase "Apan_A.dta"
erase "Atitalaquia_A.dta"
erase "Atlapexco_A.dta"
erase "Atotonilco de Tula_A.dta"
erase "Atotonilco el Grande_A.dta"
erase "Calnali_A.dta"
erase "Cardonal_A.dta"
erase "Chapantongo_A.dta"
erase "Chapulhuacan_A.dta"
erase "Chilcuahutla_A.dta"
erase "Cuahutepec de Hinojosa_A.dta"
erase "El Arenal_A.dta"
erase "Eloxochitlán_A.dta"
erase "Emiliano Zapata_A.dta"
erase "Epazoyucan_A.dta"
erase "Francisco I Madero_A.dta"
erase "Huautla_A.dta"
erase "Huasca De Ocampo_A.dta"
erase "Huazalingo_A.dta"
erase "Huehuetla_A.dta"
erase "Huejutla De Reyes_A.dta"
erase "Huichapan_A.dta"
erase "Ixmiquilpan_A.dta"
erase "Jacala de Ledezma_A.dta"
erase "Jaltocan_A.dta"
erase "Juárez Hidalgo_A.dta"
erase "La Misión_A.dta"
erase "Lolotla_A.dta"
erase "Metepec_A.dta"
erase "Metztitlán_A.dta"
erase "Mineral De La Reforma_A.dta"
erase "Mineral Del Chico_A.dta"
erase "Mineral Del Monte_A.dta"
erase "Mixquiahuala De Juárez_A.dta"
erase "Molango De Escamilla_A.dta"
erase "Nicolás Flores_A.dta"
erase "Nopala De Villagrán_A.dta"
erase "Omitlán De Juárez_A.dta"
erase "Pachuca de Soto_A.dta"
erase "Pacula_A.dta"
erase "Santiago de Anaya_A.dta"
erase "Pisaflores_A.dta"
erase "Progreso De Obregón_A.dta"
erase "San Agustín Metzquititlán_A.dta"
erase "San Agustín Tlaxiaca_A.dta"
erase "San Bartolo Tutotepec_A.dta"
erase "San Felipe Orizatlán_A.dta"
erase "San Salvador_A.dta"
erase "Santiago Tulantepec_A.dta"
erase "Singuilucan_A.dta"
erase "Tasquillo_A.dta"
erase "Tecozautla_A.dta"
erase "Tenango de Doria_A.dta"
erase "Tepeapulco_A.dta"
erase "Tepehuacan de Guerrero_A.dta"
erase "Tepeji Del Río De Ocampo_A.dta"
erase "Tepetitlán_A.dta"
erase "Tetepango_A.dta"
erase "Tezontepec De Aldama_A.dta"
erase "Tianguistengo_A.dta"
erase "Tizayuca_A.dta"
erase "Tlahuelilpan_A.dta"
erase "Tlahuiltepa_A.dta"
erase "Tlanalapa_A.dta"
erase "Tlanchinol_A.dta"
erase "Tlaxcoapan_A.dta"
erase "Tolcayuca_A.dta"
erase "Tula De Allende_A.dta"
erase "Tulancingo De Bravo_A.dta"
erase "Villa De Tezontepec_A.dta"
erase "Xochiatipan_A.dta"
erase "Xochicoatlán_A.dta"
erase "Yahualica_A.dta"
erase "Zacualtipán De Ángeles_A.dta"
erase "Zapotlán De Juárez_A.dta"
erase "Zempoala_A.dta"
erase "Zimapán_A.dta"

