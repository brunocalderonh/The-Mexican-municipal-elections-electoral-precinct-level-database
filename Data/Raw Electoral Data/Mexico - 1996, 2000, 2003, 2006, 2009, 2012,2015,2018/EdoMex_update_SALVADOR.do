////////////////////////////////////////////
////////EDO MEX PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Estado de Mexico"

///////////////////////
////////2015///////////
///////////////////////

import excel "Ayuntamientos_2015.xlsx", sheet("Aytto x secccion") clear firstrow allstring

drop Municipio uniqueid

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO DE BERRIOZABAL" | municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC DE MORELOS" | municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN DE JUAREZ" | municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA DE BAZ" | municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDAD" | municipality=="V. DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

destring *, replace

collapse (sum) PAN-total (first) coal*, by(municipality section uniqueid)

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL if coalPRIPVEMNA==1
replace PRI=. if coalPRIPVEMNA==1
replace PVEM=. if coalPRIPVEMNA==1
replace PANAL=. if coalPRIPVEMNA==1
drop PRI_PVEM PRI_PANAL PVEM_PANAL

replace PAN_PT = PAN_PT + PAN + PT if coalPANPT==1
replace PAN=. if coalPANPT==1
replace PT=. if coalPANPT==1

drop coal*

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PFD PRI_PVEM_PANAL PAN_PT CI_1 CI_2)

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PFD PRI_PVEM_PANAL PAN_PT CI_1 CI_2 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PH i_PES i_PFD i_PRI_PVEM_PANAL i_PAN_PT i_CI_1 i_CI_2, ///
	gen(PAN_r PRI_r  PRD_r  PT_r  PVEM_r  MC_r  PANAL_r  MORENA_r  PH_r  PES_r  PFD_r  PRI_PVEM_PANAL_r  PAN_PT_r CI_1_r  CI_2_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PH PES PFD PRI_PVEM_PANAL PAN_PT CI_1 CI_2 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(second, "CI_")>0
replace third="Independent" if strpos(third, "CI_")>0

gen year=2015
gen month="June"
gen STATE="ESTADO DE MEXICO"

save Mexico_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
replace winner = "PRI_PVEM_PANAL" if uniqueid==15028
rename winner incumbent

save incumbents2018.dta, replace 

preserve
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta", clear
*Lista nominal at the last date before the election (May 31st 2016) 
keep entidad municipio seccion lista file year month
keep if entidad==15 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_MEX.dta, replace
restore

use Mexico_Section_2015.dta, clear
merge 1:1 section using LN15_MEX.dta
drop if _merge==2
drop _merge
erase "LN15_MEX.dta"

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

save Mexico_Section_2015.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Mexico_2015.dta", replace

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

///////////////////////
////////2016///////////
///////////////////////

import excel "Extraordinario 2016.xlsx", sheet("Sheet1") firstrow clear

rename SECCIÓN section

replace PRI_PVEM_PANAL = PRI_PVEM_PANAL + PRI_PVEM + PRI_PANAL + PVEM_PANAL + PRI + PVEM + PANAL
drop PRI PVEM PANAL PRI_PVEM PRI_PANAL PVEM_PANAL

replace PRD_PT = PRD_PT + PRD + PT
drop PRD PT
rename ES PES

egen total = rowtotal(PAN-NULOS)

gen uniqueid = 15028

collapse (sum) PAN-PRD_PT total, by(municipality section uniqueid)

egen valid=rowtotal(PAN MC MORENA PES PFD PRI_PVEM_PANAL PRD_PT)

preserve
use "..\Listas Nominales\LN 2012-2019\2016\LN2016.dta", clear
keep if entidad==15 & month==2
keep seccion lista
rename lista listanominal
rename seccion section
save "merge.dta", replace
restore

merge 1:1 section using "merge.dta"
drop if _merge==2
drop _merge
erase "merge.dta"

gen turnout=total/listanominal

foreach var in PAN MC MORENA PES PFD PRI_PVEM_PANAL PRD_PT total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	gen i_`var'= 1/mun_`var'
}

gen mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_MC i_MORENA i_PES i_PFD i_PRI_PVEM_PANAL i_PRD_PT, gen(PAN_r MC_r MORENA_r PES_r PFD_r PRI_PVEM_PANAL_r PRD_PT_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN MC MORENA PES PFD PRI_PVEM_PANAL PRD_PT {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

gen year=2016
gen month="March"
gen STATE="ESTADO DE MEXICO"
sort uniqueid section

save Mexico_Section_2016.dta, replace

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

import excel "Ayuntamientos_2018.xlsx", sheet("Ayuntamientos") clear firstrow allstring

destring *, replace

egen indep1=rowtotal(CI_1-CI_15 CI_17 CI_18 CI_19) 
gen indep2=CI_16
drop CI_*
rename indep1 CI_1
rename indep2 CI_2

rename municipio municipality 
rename id_seccion section

gen   uniqueid= 0
replace uniqueid=15001 if municipality =="ACAMBAY"
replace uniqueid=15002 if municipality =="ACOLMAN"
replace uniqueid=15003 if municipality =="ACULCO"
replace uniqueid=15004 if municipality =="ALMOLOYA DE ALQUISIRAS"
replace uniqueid=15005 if municipality =="ALMOLOYA DE JUAREZ"
replace uniqueid=15006 if municipality =="ALMOLOYA DEL RIO"
replace uniqueid=15007 if municipality =="AMANALCO"
replace uniqueid=15008 if municipality =="AMATEPEC"
replace uniqueid=15009 if municipality =="AMECAMECA"
replace uniqueid=15010 if municipality =="APAXCO"
replace uniqueid=15011 if municipality =="ATENCO"
replace uniqueid=15012 if municipality =="ATIZAPAN"
replace uniqueid=15013 if municipality =="ATIZAPAN DE ZARAGOZA"
replace uniqueid=15014 if municipality =="ATLACOMULCO"
replace uniqueid=15015 if municipality =="ATLAUTLA"
replace uniqueid=15016 if municipality =="AXAPUSCO"
replace uniqueid=15017 if municipality =="AYAPANGO"
replace uniqueid=15018 if municipality =="CALIMAYA"
replace uniqueid=15019 if municipality =="CAPULHUAC"
replace uniqueid=15025 if municipality =="CHALCO"
replace uniqueid=15026 if municipality =="CHAPA DE MOTA"
replace uniqueid=15027 if municipality =="CHAPULTEPEC"
replace uniqueid=15028 if municipality =="CHIAUTLA"
replace uniqueid=15029 if municipality =="CHICOLOAPAN"
replace uniqueid=15030 if municipality =="CHICONCUAC"
replace uniqueid=15031 if municipality =="CHIMALHUACAN"
replace uniqueid=15020 if municipality =="COACALCO DE BERRIOZABAL" | municipality =="COACALCO"
replace uniqueid=15021 if municipality =="COATEPEC HARINAS"
replace uniqueid=15022 if municipality =="COCOTITLAN"
replace uniqueid=15023 if municipality =="COYOTEPEC"
replace uniqueid=15024 if municipality =="CUAUTITLAN"
replace uniqueid=15121 if municipality =="CUAUTITLAN IZCALLI"
replace uniqueid=15032 if municipality =="DONATO GUERRA"
replace uniqueid=15033 if municipality =="ECATEPEC DE MORELOS" | municipality =="ECATEPEC"
replace uniqueid=15034 if municipality =="ECATZINGO"
replace uniqueid=15064 if municipality =="EL ORO"
replace uniqueid=15035 if municipality =="HUEHUETOCA"
replace uniqueid=15036 if municipality =="HUEYPOXTLA"
replace uniqueid=15037 if municipality =="HUIXQUILUCAN"
replace uniqueid=15038 if municipality =="ISIDRO FABELA"
replace uniqueid=15039 if municipality =="IXTAPALUCA"
replace uniqueid=15040 if municipality =="IXTAPAN DE LA SAL"
replace uniqueid=15041 if municipality =="IXTAPAN DEL ORO"
replace uniqueid=15042 if municipality =="IXTLAHUACA"
replace uniqueid=15044 if municipality =="JALTENCO"
replace uniqueid=15045 if municipality =="JILOTEPEC"
replace uniqueid=15046 if municipality =="JILOTZINGO"
replace uniqueid=15047 if municipality =="JIQUIPILCO"
replace uniqueid=15048 if municipality =="JOCOTITLAN"
replace uniqueid=15049 if municipality =="JOQUICINGO"
replace uniqueid=15050 if municipality =="JUCHITEPEC"
replace uniqueid=15070 if municipality =="LA PAZ"
replace uniqueid=15051 if municipality =="LERMA"
replace uniqueid=15123 if municipality =="LUVIANOS"
replace uniqueid=15052 if municipality =="MALINALCO"
replace uniqueid=15053 if municipality =="MELCHOR OCAMPO"
replace uniqueid=15054 if municipality =="METEPEC"
replace uniqueid=15055 if municipality =="MEXICALTZINGO"
replace uniqueid=15056 if municipality =="MORELOS"
replace uniqueid=15057 if municipality =="NAUCALPAN DE JUAREZ" | municipality =="NAUCALPAN"
replace uniqueid=15059 if municipality =="NEXTLALPAN"
replace uniqueid=15058 if municipality =="NEZAHUALCOYOTL"
replace uniqueid=15060 if municipality =="NICOLAS ROMERO"
replace uniqueid=15061 if municipality =="NOPALTEPEC"
replace uniqueid=15062 if municipality =="OCOYOACAC"
replace uniqueid=15063 if municipality =="OCUILAN"
replace uniqueid=15065 if municipality =="OTUMBA"
replace uniqueid=15066 if municipality =="OTZOLOAPAN"
replace uniqueid=15067 if municipality =="OTZOLOTEPEC"
replace uniqueid=15068 if municipality =="OZUMBA"
replace uniqueid=15069 if municipality =="PAPALOTLA"
replace uniqueid=15071 if municipality =="POLOTITLAN"
replace uniqueid=15072 if municipality =="RAYON"
replace uniqueid=15073 if municipality =="SAN ANTONIO LA ISLA"
replace uniqueid=15074 if municipality =="SAN FELIPE DEL PROGRESO"
replace uniqueid=15124 if municipality =="SAN JOSE DEL RINCON"
replace uniqueid=15075 if municipality =="SAN MARTIN DE LAS PIRAMIDES"
replace uniqueid=15076 if municipality =="SAN MATEO ATENCO"
replace uniqueid=15077 if municipality =="SAN SIMON DE GUERRERO"
replace uniqueid=15078 if municipality =="SANTO TOMAS"
replace uniqueid=15079 if municipality =="SOYANIQUILPAN DE JUAREZ"
replace uniqueid=15080 if municipality =="SULTEPEC"
replace uniqueid=15081 if municipality =="TECAMAC"
replace uniqueid=15082 if municipality =="TEJUPILCO"
replace uniqueid=15083 if municipality =="TEMAMATLA"
replace uniqueid=15084 if municipality =="TEMASCALAPA"
replace uniqueid=15085 if municipality =="TEMASCALCINGO"
replace uniqueid=15086 if municipality =="TEMASCALTEPEC"
replace uniqueid=15087 if municipality =="TEMOAYA"
replace uniqueid=15088 if municipality =="TENANCINGO"
replace uniqueid=15089 if municipality =="TENANGO DEL AIRE"
replace uniqueid=15090 if municipality =="TENANGO DEL VALLE"
replace uniqueid=15091 if municipality =="TEOLOYUCAN"
replace uniqueid=15092 if municipality =="TEOTIHUACAN"
replace uniqueid=15093 if municipality =="TEPETLAOXTOC"
replace uniqueid=15094 if municipality =="TEPETLIXPA"
replace uniqueid=15095 if municipality =="TEPOTZOTLAN"
replace uniqueid=15096 if municipality =="TEQUIXQUIAC"
replace uniqueid=15097 if municipality =="TEXCALTITLAN"
replace uniqueid=15098 if municipality =="TEXCALYACAC"
replace uniqueid=15099 if municipality =="TEXCOCO"
replace uniqueid=15100 if municipality =="TEZOYUCA"
replace uniqueid=15101 if municipality =="TIANGUISTENCO"
replace uniqueid=15102 if municipality =="TIMILPAN"
replace uniqueid=15103 if municipality =="TLALMANALCO"
replace uniqueid=15104 if municipality =="TLALNEPANTLA DE BAZ" | municipality =="TLALNEPANTLA"
replace uniqueid=15105 if municipality =="TLATLAYA"
replace uniqueid=15106 if municipality =="TOLUCA"
replace uniqueid=15125 if municipality =="TONANITLA"
replace uniqueid=15107 if municipality =="TONATICO"
replace uniqueid=15108 if municipality =="TULTEPEC"
replace uniqueid=15109 if municipality =="TULTITLAN"
replace uniqueid=15110 if municipality =="VALLE DE BRAVO"
replace uniqueid=15122 if municipality =="VALLE DE CHALCO SOLIDARIDAD"
replace uniqueid=15111 if municipality =="VILLA DE ALLENDE"
replace uniqueid=15112 if municipality =="VILLA DEL CARBON"
replace uniqueid=15113 if municipality =="VILLA GUERRERO"
replace uniqueid=15114 if municipality =="VILLA VICTORIA"
replace uniqueid=15043 if municipality =="XALATLACO"
replace uniqueid=15115 if municipality =="XONACATLAN"
replace uniqueid=15116 if municipality =="ZACAZONAPAN"
replace uniqueid=15117 if municipality =="ZACUALPAN"
replace uniqueid=15118 if municipality =="ZINACANTEPEC"
replace uniqueid=15119 if municipality =="ZUMPAHUACAN"
replace uniqueid=15120 if municipality =="ZUMPANGO"

collapse (sum) listanominal-CI_2 (first) coalpbt coalfrente , by (municipality uniqueid section)

replace PT_MORENA_PES = PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES +  PT + MORENA + PES if coalpbt==1
replace PT=. if coalpbt==1
replace MORENA=. if coalpbt==1
replace PES=. if coalpbt==1
drop PT_MORENA PT_PES MORENA_PES

replace PAN_PRD_MC = PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC + PAN + PRD + MC if coalfrente==1
replace PAN=. if coalfrente==1
replace PRD=. if coalfrente==1
replace MC=. if coalfrente==1
drop PAN_PRD PAN_MC PRD_MC 

rename NA PANAL

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PES VIA_RADICAL PAN_PRD_MC PT_MORENA_PES CI_1 CI_2)

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES VIA_RADICAL PAN_PRD_MC PT_MORENA_PES CI_1 CI_2 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout = total/listanominal
g mun_turnout = mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PES i_VIA_RADICAL i_PAN_PRD_MC i_PT_MORENA_PES i_CI_1 i_CI_2, ///
	gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PES_r VIA_RADICAL_r PAN_PRD_MC_r PT_MORENA_PES_r CI_1_r  CI_2_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES VIA_RADICAL PAN_PRD_MC PT_MORENA_PES CI_1 CI_2 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if strpos(winner, "CI_")>0
replace second="Independent" if strpos(winner, "CI_")>0
replace third="Independent" if strpos(winner, "CI_")>0

gen year=2018
gen month="July"
gen STATE="ESTADO DE MEXICO"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta
replace incumbent = "PRI_PVEM_PANAL" if municipality=="CHIAUTLA"

g incumbent_vote = .

replace incumbent_vote=MC if incumbent=="MC" & coalfrente==0
replace incumbent_vote=PAN if incumbent=="PAN" & coalfrente==0
replace incumbent_vote=PRD if incumbent=="PRD" & coalfrente==0
replace incumbent_vote=PAN_PRD_MC if (incumbent=="PAN" | incumbent=="PRD" | incumbent=="MC")  & coalfrente==1

replace incumbent_vote=PT if incumbent=="PT"  & coalpbt==0
replace incumbent_vote=PES if incumbent=="PES"  & coalpbt==0
replace incumbent_vote=MORENA if incumbent=="MORENA"  & coalpbt==0
replace incumbent_vote=PT_MORENA_PES if coalpbt==1 & (incumbent=="PT" | incumbent=="MORENA" | incumbent=="PES")

replace incumbent_vote=PRI if incumbent=="PRI" | incumbent=="PRI_PVEM_PANAL"
replace incumbent_vote=PVEM if incumbent=="PVEM" 
replace incumbent_vote=PANAL if incumbent=="PANAL" | (incumbent=="PRI_PVEM_PANAL" & (uniqueid==15050 | uniqueid==15025) )

replace incumbent_vote=PAN if incumbent=="PAN_PT" & coalfrente==0 
replace incumbent_vote=PAN_PRD_MC if incumbent=="PAN_PT" & coalfrente==1 

replace incumbent_vote=PT if (uniqueid==15028 | uniqueid==15116) & coalpbt==0
replace incumbent_vote=PT_MORENA_PES if (uniqueid==15028 | uniqueid==15116) & coalpbt==1

drop coal* no_reg nulo

save Mexico_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) listanominal-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid *
save "..\..\Update Municipal\Mexico_2018.dta", replace

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

clear
append using Mexico_Section_2018.dta
append using Mexico_Section_2016.dta
append using Mexico_Section_2015.dta

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

save Mexico_Section_15_18.dta, replace

use "..\..\Precinct\Mexico_ALL.dta", clear
append using Mexico_Section_15_18.dta
save Mexico_ALL_SALVADOR.dta, replace

erase "Mexico_Section_2018.dta"
erase "Mexico_Section_2016.dta"
erase "Mexico_Section_2015.dta"
erase "Mexico_Section_15_18.dta"
