
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Michoacan - 1995, 1998, 2001, 2004, 2007, 2011"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Michoacan - 1995, 1998, 2001, 2004, 2007, 2011"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1995_No_LN.csv, clear

rename municipio  municipality
rename seccin section

drop if municipality=="" & section==.

destring pan -  nulos, replace

collapse (sum) pan-  nulos, by (municipality section)

gen total =  pan + pri  + prd  + pfcrn  + pt  + noreg  + nulos

* br if total==0 
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pfcrn  Partido_Cardenista
rename  pt     PT

* gen turnout =  total/listanominal

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=16001 if municipality =="ACUITZIO"
replace uniqueid=16002 if municipality =="AGUILILLA"
replace uniqueid=16003 if municipality =="A. OBREGON."
replace uniqueid=16004 if municipality =="ANGAMACUTIRO"
replace uniqueid=16005 if municipality =="ANGANGUEO"
replace uniqueid=16006 if municipality =="APATZINGAN"
replace uniqueid=16007 if municipality =="APORO"
replace uniqueid=16008 if municipality =="AQUILA"
replace uniqueid=16009 if municipality =="ARIO DE R."
replace uniqueid=16010 if municipality =="ARTEAGA"
replace uniqueid=16011 if municipality =="BRISEÑAS"
replace uniqueid=16012 if municipality =="BUENAVISTA"
replace uniqueid=16013 if municipality =="CARACUARO"
replace uniqueid=16021 if municipality =="CHARAPAN"
replace uniqueid=16022 if municipality =="CHARO"
replace uniqueid=16023 if municipality =="CHAVINDA"
replace uniqueid=16024 if municipality =="CHERAN"
replace uniqueid=16025 if municipality =="CHILCHOTA"
replace uniqueid=16026 if municipality =="CHINICUILA"
replace uniqueid=16027 if municipality =="CHUCANDIRO"
replace uniqueid=16028 if municipality =="CHURINTZIO"
replace uniqueid=16029 if municipality =="CHURUMUCO"
replace uniqueid=16014 if municipality =="COAHUAYANA"
replace uniqueid=16015 if municipality =="COALCOMAN"
replace uniqueid=16016 if municipality =="COENEO"
replace uniqueid=16074 if municipality =="REGULES"
replace uniqueid=16017 if municipality =="CONTEPEC"
replace uniqueid=16018 if municipality =="COPANDARO"
replace uniqueid=16019 if municipality =="COTIJA"
replace uniqueid=16020 if municipality =="CUITZEO"
replace uniqueid=16030 if municipality =="ECUANDUREO"
replace uniqueid=16031 if municipality =="E. HUERTA"
replace uniqueid=16032 if municipality =="ERONGARI."
replace uniqueid=16033 if municipality =="GABRIEL ZAMORA"
replace uniqueid=16034 if municipality =="HIDALGO"
replace uniqueid=16036 if municipality =="HUANDACAREO"
replace uniqueid=16037 if municipality =="HUANIQUEO"
replace uniqueid=16038 if municipality =="HUETAMO"
replace uniqueid=16039 if municipality =="HUIRAMBA"
replace uniqueid=16040 if municipality =="INDAPARAPEO"
replace uniqueid=16041 if municipality =="IRIMBO"
replace uniqueid=16042 if municipality =="IXTLAN"
replace uniqueid=16043 if municipality =="JACONA"
replace uniqueid=16044 if municipality =="JIMENEZ"
replace uniqueid=16045 if municipality =="JIQUILPAN"
replace uniqueid=16046 if municipality =="JUAREZ"
replace uniqueid=16047 if municipality =="JUNGAPEO"
replace uniqueid=16035 if municipality =="LA HUACANA"
replace uniqueid=16069 if municipality =="LA PIEDAD"
replace uniqueid=16048 if municipality =="LAGUNILLAS"
replace uniqueid=16052 if municipality =="L. CARDENAS"
replace uniqueid=16075 if municipality =="LOS REYES"
replace uniqueid=16049 if municipality =="MADERO"
replace uniqueid=16050 if municipality =="MARAVATIO"
replace uniqueid=16051 if municipality =="M.CASTELLANOS"
replace uniqueid=16053 if municipality =="MORELIA"
replace uniqueid=16054 if municipality =="MORELOS"
replace uniqueid=16055 if municipality =="MUGICA"
replace uniqueid=16056 if municipality =="NAHUATZEN"
replace uniqueid=16057 if municipality =="NOCUPETARO"
replace uniqueid=16058 if municipality =="NUEVO PARANGARI"
replace uniqueid=16059 if municipality =="NUEVO URECHO"
replace uniqueid=16060 if municipality =="NUMARAN"
replace uniqueid=16061 if municipality =="OCAMPO"
replace uniqueid=16062 if municipality =="PAJACUARAN"
replace uniqueid=16063 if municipality =="PANINDICUARO"
replace uniqueid=16065 if municipality =="PARACHO"
replace uniqueid=16064 if municipality =="PARACUARO"
replace uniqueid=16066 if municipality =="PATZCUARO"
replace uniqueid=16067 if municipality =="PENJAMILLO"
replace uniqueid=16068 if municipality =="PERIBAN"
replace uniqueid=16070 if municipality =="PUREPERO"
replace uniqueid=16071 if municipality =="PURUANDIRO"
replace uniqueid=16072 if municipality =="QUERENDARO"
replace uniqueid=16073 if municipality =="QUIROGA"
replace uniqueid=16076 if municipality =="SAHUAYO"
replace uniqueid=16079 if municipality =="SALV. ESC."
replace uniqueid=16077 if municipality =="SAN LUCAS"
replace uniqueid=16078 if municipality =="STA. ANA M."
replace uniqueid=16080 if municipality =="SENGUIO"
replace uniqueid=16081 if municipality =="SUSUPUATO"
replace uniqueid=16082 if municipality =="TACAMBARO"
replace uniqueid=16083 if municipality =="TANCITARO"
replace uniqueid=16084 if municipality =="TANGAMANDAP."
replace uniqueid=16085 if municipality =="TANGANCI"
replace uniqueid=16086 if municipality =="TANHUATO"
replace uniqueid=16087 if municipality =="TARETAN"
replace uniqueid=16088 if municipality =="TARIMBARO"
replace uniqueid=16089 if municipality =="TEPALCATEP."
replace uniqueid=16090 if municipality =="TINGAMBATO"
replace uniqueid=16091 if municipality =="TINGUINDIN"
replace uniqueid=16092 if municipality =="TIQUICHEO"
replace uniqueid=16093 if municipality =="TLALPUJAHUA"
replace uniqueid=16094 if municipality =="TLAZAZALCA"
replace uniqueid=16095 if municipality =="TOCUMBO"
replace uniqueid=16096 if municipality =="TUMBISCATIO"
replace uniqueid=16097 if municipality =="TURICATO"
replace uniqueid=16098 if municipality =="TUXPAN"
replace uniqueid=16099 if municipality =="TUZANTLA"
replace uniqueid=16100 if municipality =="TZINTZUNTZAN"
replace uniqueid=16101 if municipality =="TZITZIO"
replace uniqueid=16102 if municipality =="URUAPAN"
replace uniqueid=16103 if municipality =="V. CARRANZA"
replace uniqueid=16104 if municipality =="VILLAMAR"
replace uniqueid=16105 if municipality =="VISTA HERMOSA"
replace uniqueid=16106 if municipality =="YURECUARO"
replace uniqueid=16107 if municipality =="ZACAPU"
replace uniqueid=16108 if municipality =="ZAMORA"
replace uniqueid=16109 if municipality =="ZINAPARO"
replace uniqueid=16110 if municipality =="ZINAPECUARO"
replace uniqueid=16111 if municipality =="ZIRACUARE"
replace uniqueid=16112 if municipality =="ZITACUARO"
replace uniqueid=16113 if municipality =="JOSE SIXTO VERDUZCO"

egen valid = rowtotal(PAN PRI PRD Partido_Cardenista PT)

foreach var in PAN PRI PRD Partido_Cardenista PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_Partido_Cardenista inv_mun_PT, gen(PAN_r PRI_r PRD_r Partido_Cardenista_r PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD Partido_Cardenista PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1995
gen month ="November"

save Michoacan_Section_1995.dta, replace

keep municipality section uniqueid

sort section 

save Michoacan_Section_1995_to_Merge_with_1998.dta, replace

use Michoacan_Section_1995.dta, clear
drop if winner==""
save Michoacan_Section_1995.dta, replace

*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1998_Half_Missing.csv, clear

rename municipio  municipality
rename listanom listanominal
rename seccion section

drop if municipality==. & section==.
drop if total==. | total==0 

destring pan -  total, replace

collapse (sum) pan -  total listanomina, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM

gen turnout =  total/listanominal

drop   noregistrados nulos 




egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total listanominal valid{
bys municipality: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1998
gen month ="November"

drop municipality

sort section
merge section using Michoacan_Section_1995_to_Merge_with_1998.dta
drop if _merge==2
drop _merge
replace uniqueid = 16051 if section ==935 & uniqueid==.
replace municipality = "M.CASTELLANOS" if section ==935 & uniqueid==16051

replace uniqueid = 16053 if section ==2675 & uniqueid==.
replace municipality = "MORELIA" if section ==2675 & uniqueid==16053

save Michoacan_Section_1998.dta, replace


*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2001.csv, clear

rename  nombre_municipio  municipality

rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan -  total, replace

collapse (sum) listanominal pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prdptpvempaspsnconvergencia PRD_PT_PC_PVEM_PAS_PSN

gen turnout =  total/listanominal

drop    noreg nulos 

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
replace uniqueid=16011 if municipality =="Brise?as"
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
replace uniqueid=16015 if municipality =="Coalcoman"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="Regules"
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
replace uniqueid=16038 if municipality =="Huatamo"
replace uniqueid=16039 if municipality =="Huiramba"
replace uniqueid=16040 if municipality =="Indaparapeo"
replace uniqueid=16041 if municipality =="Irimbo"
replace uniqueid=16042 if municipality =="Ixtlan"
replace uniqueid=16043 if municipality =="Jacona"
replace uniqueid=16044 if municipality =="Jimenez"
replace uniqueid=16045 if municipality =="Jiquilpan"
replace uniqueid=16113 if municipality =="Jose Sixto Verduzco"
replace uniqueid=16046 if municipality =="Juarez"
replace uniqueid=16047 if municipality =="Juagapeo"
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
replace uniqueid=16091 if municipality =="Tinguindin"
replace uniqueid=16092 if municipality =="Tiquicheo"
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

egen valid = rowtotal(PAN PRI PRD_PT_PC_PVEM_PAS_PSN)

foreach var in PAN PRI PRD_PT_PC_PVEM_PAS_PSN total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT_PC_PVEM_PAS_PSN, gen(PAN_r PRI_r PRD_PT_PC_PVEM_PAS_PSN_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT_PC_PVEM_PAS_PSN {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year=2001
gen month ="November" 

save Michoacan_Section_2001.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2004.csv, clear 

rename  nombre_municipio  municipality
rename seccion section
rename  lista_nominal listanominal
drop if municipality=="Ario" & section==170

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan -  total, replace

collapse (sum) listanominal pan -  pc total, by (municipality section)

rename  pan    PAN
rename  pripvem    PRI_PVEM
rename  prd    PRD
rename  pt     PT
rename  pc     PC

gen turnout =  total/listanominal

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
replace uniqueid=16011 if municipality =="Brise?as"
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
replace uniqueid=16015 if municipality =="Coalcoman"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="Regules"
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
replace uniqueid=16091 if municipality =="Tinguindin"
replace uniqueid=16092 if municipality =="Tiquicheo"
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

egen valid = rowtotal(PAN PRI_PVEM PRD PT PC)

foreach var in PAN PRI_PVEM PRD PT PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PC, gen(PAN_r PRI_PVEM_r PRD_r PT_r PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM PRD PT PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2004
gen month ="November"

save Michoacan_Section_2004.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2007.csv, clear

rename  municipio  municipality
rename  seccion section

drop if municipality=="" & section==.

egen total = rowtotal(pan pri prd pt pvem pc pna pas coal_prd_pt coal_prd_pt_pc coal_prd_pc cc_pan_pri cc_pan_pri_pvem cc_pan_pvem  cc_pan_pvem_pna cc_pan_pna cc_pri_pvem cc_pri_pvem_pna noreg nulos)
drop if total==. | total==0 

destring listanominal -  cc_pri_pvem_pna total, replace

collapse (sum) listanominal -  cc_pri_pvem_pna total, by (municipality section)

*************************************************************************************************

*** Coalitions ***
rename  coal_prd_pt prd_pt 

replace coal_prd_pt_pc = prd + pt + pc + coal_prd_pt_pc if coal_prd_pt_pc>0
replace prd =0 if coal_prd_pt_pc>0
replace pt =0 if coal_prd_pt_pc>0
replace pc =0 if coal_prd_pt_pc>0
rename  coal_prd_pt_pc prd_pt_pc
 
rename coal_prd_pc prd_pc 

*** Common candidates ***

replace cc_pan_pvem = cc_pan_pvem + pan + pvem if cc_pan_pvem > 0
replace pan =0  if cc_pan_pvem > 0
replace pvem =0 if cc_pan_pvem > 0
rename  cc_pan_pvem pan_pvem

replace cc_pan_pvem_pna = cc_pan_pvem_pna + pan + pvem + pna if cc_pan_pvem_pna > 0
replace pan =0  if cc_pan_pvem_pna > 0
replace pvem =0 if cc_pan_pvem_pna > 0
replace pna =0 if cc_pan_pvem_pna > 0
rename  cc_pan_pvem_pna pan_pvem_pna
 
replace cc_pan_pna  = cc_pan_pna  + pan + pna if cc_pan_pna  > 0
replace pan =0  if cc_pan_pna  > 0
replace pna =0 if cc_pan_pna  > 0
rename  cc_pan_pna pan_pna
 
replace cc_pan_pri = pan + pri + cc_pan_pri if cc_pan_pri>0
replace pan = 0  if cc_pan_pri>0
replace pri= 0  if cc_pan_pri>0
rename cc_pan_pri pan_pri 
 
replace cc_pan_pri_pvem = pan + pri + pvem + cc_pan_pri_pvem if cc_pan_pri_pvem>0
replace pan = 0  if cc_pan_pri_pvem>0
replace pri = 0  if cc_pan_pri_pvem>0
replace pvem = 0  if cc_pan_pri_pvem>0
rename cc_pan_pri_pvem  pan_pri_pvem
 
replace cc_pri_pvem = pri + pvem + cc_pri_pvem if cc_pri_pvem>0
replace pri = 0  if cc_pri_pvem>0
replace pvem = 0  if cc_pri_pvem>0
rename cc_pri_pvem pri_pvem 
  
replace cc_pri_pvem_pna = pri + pvem + pna + cc_pri_pvem_pna if cc_pri_pvem_pna>0
replace pri = 0  if cc_pri_pvem_pna>0
replace pvem = 0  if cc_pri_pvem_pna>0
replace pna= 0  if cc_pri_pvem_pna>0
rename  cc_pri_pvem_pna pri_pvem_pna 

*************************************************************************************************
rename pan PAN
rename pri PRI
rename prd  PRD
rename pt  PT
rename pvem  PVEM
rename pc PC
rename pna PANAL
rename pas PAS
rename prd_pt PRD_PT
rename prd_pt_pc PRD_PT_PC
rename prd_pc PRD_PC
rename pan_pvem PAN_PVEM
rename pan_pvem_pna PAN_PVEM_PANAL
rename pan_pna PAN_PANAL
rename pan_pri PAN_PRI
rename pan_pri_pvem PAN_PRI_PVEM
rename pri_pvem PRI_PVEM
rename pri_pvem_pna PRI_PVEM_PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=16001 if municipality =="Acuitzio"
replace uniqueid=16002 if municipality =="Aguililla"
replace uniqueid=16003 if municipality =="Alvaro Obregón"
replace uniqueid=16004 if municipality =="Angamacutiro"
replace uniqueid=16005 if municipality =="Angangueo"
replace uniqueid=16006 if municipality =="Apatzingán"
replace uniqueid=16007 if municipality =="Aporo"
replace uniqueid=16008 if municipality =="Aquila"
replace uniqueid=16009 if municipality =="Ario"
replace uniqueid=16010 if municipality =="Arteaga"
replace uniqueid=16011 if municipality =="Briseñas"
replace uniqueid=16012 if municipality =="Buenavista"
replace uniqueid=16013 if municipality =="Carácuaro"
replace uniqueid=16021 if municipality =="Charapan"
replace uniqueid=16022 if municipality =="Charo"
replace uniqueid=16023 if municipality =="Chavinda"
replace uniqueid=16024 if municipality =="Cherán"
replace uniqueid=16025 if municipality =="Chilchota"
replace uniqueid=16026 if municipality =="Chinicuila"
replace uniqueid=16027 if municipality =="Chucándiro"
replace uniqueid=16028 if municipality =="Churintzio"
replace uniqueid=16029 if municipality =="Churumuco"
replace uniqueid=16014 if municipality =="Coahuayana"
replace uniqueid=16015 if municipality =="Coalcomán"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="Régules"
replace uniqueid=16017 if municipality =="Contepec"
replace uniqueid=16018 if municipality =="Copándaro"
replace uniqueid=16019 if municipality =="Cotija"
replace uniqueid=16020 if municipality =="Cuitzeo"
replace uniqueid=16030 if municipality =="Ecuandureo"
replace uniqueid=16031 if municipality =="Epitacio Huerta"
replace uniqueid=16032 if municipality =="Erongarícuaro"
replace uniqueid=16033 if municipality =="Gabriel Zamora"
replace uniqueid=16034 if municipality =="Hidalgo"
replace uniqueid=16036 if municipality =="Huandacareo"
replace uniqueid=16037 if municipality =="Huaníqueo"
replace uniqueid=16038 if municipality =="Huetamo"
replace uniqueid=16039 if municipality =="Huiramba"
replace uniqueid=16040 if municipality =="Indaparapeo"
replace uniqueid=16041 if municipality =="Irimbo"
replace uniqueid=16042 if municipality =="Ixtlán"
replace uniqueid=16043 if municipality =="Jacona"
replace uniqueid=16044 if municipality =="Jiménez"
replace uniqueid=16045 if municipality =="Jiquilpan"
replace uniqueid=16113 if municipality =="José Sixto Verduzco"
replace uniqueid=16046 if municipality =="Juárez"
replace uniqueid=16047 if municipality =="Jungapeo"
replace uniqueid=16035 if municipality =="La Huacana"
replace uniqueid=16069 if municipality =="La Piedad"
replace uniqueid=16048 if municipality =="Lagunillas"
replace uniqueid=16052 if municipality =="Lázaro Cárdenas"
replace uniqueid=16075 if municipality =="Los Reyes"
replace uniqueid=16049 if municipality =="Madero"
replace uniqueid=16050 if municipality =="Maravatio"
replace uniqueid=16051 if municipality =="Marcos Castellanos"
replace uniqueid=16053 if municipality =="Morelia"
replace uniqueid=16054 if municipality =="Morelos"
replace uniqueid=16055 if municipality =="Múgica"
replace uniqueid=16056 if municipality =="Nahuátzen"
replace uniqueid=16057 if municipality =="Nocupétaro"
replace uniqueid=16058 if municipality =="Nuevo Parangaricutiro"
replace uniqueid=16059 if municipality =="Nuevo Urecho"
replace uniqueid=16060 if municipality =="Numarán"
replace uniqueid=16061 if municipality =="Ocampo"
replace uniqueid=16062 if municipality =="Pajacuarán"
replace uniqueid=16063 if municipality =="Panindícuaro"
replace uniqueid=16065 if municipality =="Paracho"
replace uniqueid=16064 if municipality =="Parácuaro"
replace uniqueid=16066 if municipality =="Pátzcuaro"
replace uniqueid=16067 if municipality =="Penjamillo"
replace uniqueid=16068 if municipality =="Peribán"
replace uniqueid=16070 if municipality =="Purépero"
replace uniqueid=16071 if municipality =="Puruándiro"
replace uniqueid=16072 if municipality =="Queréndaro"
replace uniqueid=16073 if municipality =="Quiroga"
replace uniqueid=16076 if municipality =="Sahuayo"
replace uniqueid=16079 if municipality =="Salvador Escalante"
replace uniqueid=16077 if municipality =="San Lucas"
replace uniqueid=16078 if municipality =="Santa Ana Maya"
replace uniqueid=16080 if municipality =="Senguio"
replace uniqueid=16081 if municipality =="Susupuato"
replace uniqueid=16082 if municipality =="Tacámbaro"
replace uniqueid=16083 if municipality =="Tancítaro"
replace uniqueid=16084 if municipality =="Tangamandapio"
replace uniqueid=16085 if municipality =="Tangancícuaro"
replace uniqueid=16086 if municipality =="Tanhuato"
replace uniqueid=16087 if municipality =="Taretan"
replace uniqueid=16088 if municipality =="Tarímbaro"
replace uniqueid=16089 if municipality =="Tepalcatepec"
replace uniqueid=16090 if municipality =="Tingambato"
replace uniqueid=16091 if municipality =="Tingüindín"
replace uniqueid=16092 if municipality =="Tiquicheo"
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
replace uniqueid=16106 if municipality =="Yurécuaro"
replace uniqueid=16107 if municipality =="Zacapu"
replace uniqueid=16108 if municipality =="Zamora"
replace uniqueid=16109 if municipality =="Zináparo"
replace uniqueid=16110 if municipality =="Zinapécuaro"
replace uniqueid=16111 if municipality =="Ziracuaretiro"
replace uniqueid=16112 if municipality =="Zitácuaro"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAS PRD_PT PRD_PT_PC PRD_PC PAN_PRI PAN_PRI_PVEM PAN_PVEM PAN_PVEM_PANAL PAN_PANAL PRI_PVEM PRI_PVEM_PANAL)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAS PRD_PT PRD_PT_PC PRD_PC PAN_PRI PAN_PRI_PVEM PAN_PVEM PAN_PVEM_PANAL PAN_PANAL PRI_PVEM PRI_PVEM_PANAL total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAS inv_mun_PRD_PT inv_mun_PRD_PT_PC inv_mun_PRD_PC inv_mun_PAN_PRI inv_mun_PAN_PRI_PVEM inv_mun_PAN_PVEM inv_mun_PAN_PVEM_PANAL inv_mun_PAN_PANAL inv_mun_PRI_PVEM inv_mun_PRI_PVEM_PANAL, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAS_r PRD_PT_r PRD_PT_PC_r PRD_PC_r PAN_PRI_r PAN_PRI_PVEM_r PAN_PVEM_r PAN_PVEM_PANAL_r PAN_PANAL_r PRI_PVEM_r PRI_PVEM_PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PANAL PAS PRD_PT PRD_PT_PC PRD_PC PAN_PRI PAN_PRI_PVEM PAN_PVEM PAN_PVEM_PANAL PAN_PANAL PRI_PVEM PRI_PVEM_PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}


drop *_r

gen year = 2007
gen month ="November"

save Michoacan_Section_2007.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

use Ayu_Seccion_2011.dta, clear

replace municipality = proper(municipality)

gen turnout =  total/listanominal

drop noreg nulos

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
replace uniqueid=16011 if municipality =="Brisenas"
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
replace uniqueid=16015 if municipality =="Coalcoman"
replace uniqueid=16016 if municipality =="Coeneo"
replace uniqueid=16074 if municipality =="Regules"
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
replace uniqueid=16091 if municipality =="Tinguindin"
replace uniqueid=16092 if municipality =="Tiquicheo"
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

sum PRD_PT  PRD PT if C_PRD_PT>0 & C_PRD_PT!=.
replace PRD_PT = C_PRD_PT if C_PRD_PT !=0 & PRD_PT==0
drop C_PRD_PT

foreach var in PAN_PRI_PVEM PAN_PRI_PVEM_PANAL PAN_PRI_PANAL PAN_PANAL PRI_PRD_PVEM_PC_PANAL PRI_PVEM PRD_PT PRD_PT_PC PRD_PC PT_PC {
bys uniqueid: egen c_`var' = sum(`var')
replace c_`var' = (c_`var'>0)
}

replace PAN_PRI_PVEM =  PAN_PRI_PVEM  + PAN + PRI + PVEM  if c_PAN_PRI_PVEM==1
replace PAN = 0 if c_PAN_PRI_PVEM==1
replace PRI = 0 if c_PAN_PRI_PVEM==1
replace PVEM = 0 if c_PAN_PRI_PVEM==1

replace PAN_PRI_PVEM_PANAL =  PAN_PRI_PVEM_PANAL  + PAN + PRI + PVEM  + PANAL if c_PAN_PRI_PVEM_PANAL==1
replace PAN = 0 if c_PAN_PRI_PVEM_PANAL==1
replace PRI = 0 if c_PAN_PRI_PVEM_PANAL==1
replace PVEM = 0 if c_PAN_PRI_PVEM_PANAL==1
replace PANAL = 0 if c_PAN_PRI_PVEM_PANAL==1
 
replace PAN_PRI_PANAL =  PAN_PRI_PANAL  + PAN + PRI  + PANAL if c_PAN_PRI_PANAL==1
replace PAN = 0 if c_PAN_PRI_PANAL==1
replace PRI = 0 if c_PAN_PRI_PANAL==1
replace PANAL = 0 if c_PAN_PRI_PANAL==1
 
replace PAN_PANAL =  PAN_PANAL  + PAN + PANAL if c_PAN_PANAL==1
replace PAN = 0 if c_PAN_PANAL==1
replace PANAL = 0 if c_PAN_PANAL==1
 
replace PRI_PRD_PVEM_PC_PANAL  =  PRI_PRD_PVEM_PC_PANAL + PRI + PRD + PVEM + PC + PANAL  if c_PRI_PRD_PVEM_PC_PANAL ==1
replace PRI = 0 if c_PRI_PRD_PVEM_PC_PANAL ==1
replace PRD = 0 if c_PRI_PRD_PVEM_PC_PANAL ==1
replace PVEM = 0 if c_PRI_PRD_PVEM_PC_PANAL ==1
replace PC = 0 if c_PRI_PRD_PVEM_PC_PANAL ==1
replace PANAL = 0 if c_PRI_PRD_PVEM_PC_PANAL ==1 
 
replace PRI_PVEM  = PRI_PVEM + PRI + PVEM if c_PRI_PVEM ==1
replace PRI = 0 if c_PRI_PVEM ==1
replace PVEM = 0 if c_PRI_PVEM ==1

replace PRD_PT  = PRD_PT + PRD + PT if c_PRD_PT ==1
replace PRD = 0 if c_PRD_PT ==1
replace PT = 0 if c_PRD_PT ==1

replace PRD_PT_PC  = PRD_PT_PC + PRD + PT + PC if c_PRD_PT_PC ==1
replace PRD = 0 if c_PRD_PT_PC ==1
replace PT = 0 if c_PRD_PT_PC ==1
replace PC = 0 if c_PRD_PT_PC ==1
 
replace PRD_PC  = PRD_PC + PRD + PC if c_PRD_PC ==1
replace PRD = 0 if c_PRD_PC ==1
replace PC = 0 if c_PRD_PC ==1
 
replace PT_PC  = PT_PC + PT + PC if c_PT_PC ==1
replace PT = 0 if c_PT_PC ==1
replace PC = 0 if c_PT_PC ==1
drop c_*

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PRI_PVEM PAN_PRI_PVEM_PANAL PAN_PRI_PANAL PAN_PANAL PRI_PRD_PVEM_PC_PANAL PRI_PVEM PRD_PT PRD_PT_PC PRD_PC PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PRI_PVEM PAN_PRI_PVEM_PANAL PAN_PRI_PANAL PAN_PANAL PRI_PRD_PVEM_PC_PANAL PRI_PVEM PRD_PT PRD_PT_PC PRD_PC PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAN_PRI_PVEM inv_mun_PAN_PRI_PVEM_PANAL inv_mun_PAN_PRI_PANAL inv_mun_PAN_PANAL inv_mun_PRI_PRD_PVEM_PC_PANAL inv_mun_PRI_PVEM inv_mun_PRD_PT inv_mun_PRD_PT_PC inv_mun_PRD_PC inv_mun_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAN_PRI_PVEM_r PAN_PRI_PVEM_PANAL_r PAN_PRI_PANAL_r PAN_PANAL_r PRI_PRD_PVEM_PC_PANAL_r PRI_PVEM_r PRD_PT_r PRD_PT_PC_r PRD_PC_r PT_PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PRI_PVEM PAN_PRI_PVEM_PANAL PAN_PRI_PANAL PAN_PANAL PRI_PRD_PVEM_PC_PANAL PRI_PVEM PRD_PT PRD_PT_PC PRD_PC PT_PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2011
gen month ="November"

save Michoacan_Section_2011.dta, replace


*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Michoacan_Section_1995.dta
append using Michoacan_Section_1998.dta
append using Michoacan_Section_2001.dta
append using Michoacan_Section_2004.dta
append using Michoacan_Section_2007.dta
append using Michoacan_Section_2011.dta


erase Michoacan_Section_1995.dta
erase Michoacan_Section_1998.dta
erase Michoacan_Section_2001.dta
erase Michoacan_Section_2004.dta
erase Michoacan_Section_2007.dta
erase Michoacan_Section_2011.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Michoacan_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Michoacan_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
