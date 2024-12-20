

clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Jalisco - 1995, 1997, 2000, 2003, 2006, 2009, 2012"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1997_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
*drop if total==. | total==0

destring pan -  total, replace
collapse (sum) pan-  total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM
rename  pps    PPS
rename  pdm    PDM

* gen turnout =  total/listanominal

drop   validos noreg nulos 

gen   uniqueid= 0
replace uniqueid=14001 if municipality =="Acatic"
replace uniqueid=14002 if municipality =="Acatlán de Juárez"
replace uniqueid=14003 if municipality =="Ahualulco de Mercado"
replace uniqueid=14004 if municipality =="Amacueca"
replace uniqueid=14005 if municipality =="Amatitan"
replace uniqueid=14006 if municipality =="Ameca"
replace uniqueid=14008 if municipality =="Arandas"
replace uniqueid=14010 if municipality =="Atemajac de Brizuela"
replace uniqueid=14011 if municipality =="Atengo"
replace uniqueid=14012 if municipality =="Atenguillo"
replace uniqueid=14013 if municipality =="Atotonilco el Alto"
replace uniqueid=14014 if municipality =="Atoyac"
replace uniqueid=14015 if municipality =="Autlán de Navarro"
replace uniqueid=14016 if municipality =="Ayotlán"
replace uniqueid=14017 if municipality =="Ayutla"
replace uniqueid=14019 if municipality =="Bolaños"
replace uniqueid=14020 if municipality =="Cabo Corrientes"
replace uniqueid=14117 if municipality =="Villa Obregón"
replace uniqueid=14021 if municipality =="Casimiro Castillo"
replace uniqueid=14030 if municipality =="Chapala"
replace uniqueid=14031 if municipality =="Chimaltitán"
replace uniqueid=14032 if municipality =="Chiquilistlán"
replace uniqueid=14022 if municipality =="Cihuatlán"
replace uniqueid=14024 if municipality =="Cocula"
replace uniqueid=14025 if municipality =="Colotlán"
replace uniqueid=14026 if municipality =="Concepción de Buenos Aires"
replace uniqueid=14027 if municipality =="Cuautitlán"
replace uniqueid=14028 if municipality =="Cuautla"
replace uniqueid=14029 if municipality =="Cuquio"
replace uniqueid=14033 if municipality =="Degollado"
replace uniqueid=14034 if municipality =="Ejutla"
replace uniqueid=14009 if municipality =="El Arenal"
replace uniqueid=14037 if municipality =="El Grullo"
replace uniqueid=14054 if municipality =="El Limón"
replace uniqueid=14070 if municipality =="El Salto"
replace uniqueid=14035 if municipality =="Encarnación de Díaz"
replace uniqueid=14036 if municipality =="Etzatlan"
replace uniqueid=14079 if municipality =="Gómez Farías"
replace uniqueid=14038 if municipality =="Guachinango"
replace uniqueid=14039 if municipality =="Guadalajara"
replace uniqueid=14040 if municipality =="Hostotipaquillo"
replace uniqueid=14041 if municipality =="Huejucar"
replace uniqueid=14042 if municipality =="Huejuquilla el Alto"
replace uniqueid=14044 if municipality =="Ixtlahuacan de los Membrillos"
replace uniqueid=14045 if municipality =="Ixtlahuacán del Río"
replace uniqueid=14046 if municipality =="Jalostotitlán"
replace uniqueid=14047 if municipality =="Jamay"
replace uniqueid=14048 if municipality =="Jesús María"
replace uniqueid=14049 if municipality =="Jilotlán de los Dolores"
replace uniqueid=14050 if municipality =="Jocotepec"
replace uniqueid=14051 if municipality =="Juanacatlán"
replace uniqueid=14052 if municipality =="Juchitlán"
replace uniqueid=14018 if municipality =="La Barca"
replace uniqueid=14043 if municipality =="La Huerta"
replace uniqueid=14057 if municipality =="Manzanilla de la Paz"
replace uniqueid=14053 if municipality =="Lagos de Moreno"
replace uniqueid=14055 if municipality =="Magdalena"
replace uniqueid=14058 if municipality =="Mascota"
replace uniqueid=14059 if municipality =="Mazamitla"
replace uniqueid=14060 if municipality =="Mexticacan"
replace uniqueid=14061 if municipality =="Mezquitic"
replace uniqueid=14062 if municipality =="Mixtlan"
replace uniqueid=14063 if municipality =="Ocotlán"
replace uniqueid=14064 if municipality =="Ojuelos de Jalisco"
replace uniqueid=14065 if municipality =="Pihuamo"
replace uniqueid=14066 if municipality =="Poncitlán"
replace uniqueid=14067 if municipality =="Puerto Vallarta"
replace uniqueid=14069 if municipality =="Quitupán"
replace uniqueid=14071 if municipality =="San Cristobal de la Barranca"
replace uniqueid=14072 if municipality =="San Diego de Alejandria"
replace uniqueid=14113 if municipality =="San Gabriel"
replace uniqueid=14073 if municipality =="San Juan de los Lagos"
replace uniqueid=14007 if municipality =="Antonio Escobedo"
replace uniqueid=14074 if municipality =="San Julián"
replace uniqueid=14075 if municipality =="San Marcos"
replace uniqueid=14076 if municipality =="San Martín de Bolaños"
replace uniqueid=14077 if municipality =="San Martín Hidalgo"
replace uniqueid=14078 if municipality =="San Miguel el Alto"
replace uniqueid=14080 if municipality =="San Sebastián del Oeste"
replace uniqueid=14081 if municipality =="Santa María de los Angeles"
replace uniqueid=14056 if municipality =="Manuel M.Dieguez"
replace uniqueid=14082 if municipality =="Sayula"
replace uniqueid=14083 if municipality =="Tala"
replace uniqueid=14084 if municipality =="Talpa de Allende"
replace uniqueid=14085 if municipality =="Tamazula de Gordiano"
replace uniqueid=14086 if municipality =="Tapalpa"
replace uniqueid=14087 if municipality =="Tecalitlán"
replace uniqueid=14089 if municipality =="Techaluta de Montenegro"
replace uniqueid=14088 if municipality =="Tecolotlán"
replace uniqueid=14090 if municipality =="Tenamaxtlán"
replace uniqueid=14091 if municipality =="Teocaltiche"
replace uniqueid=14092 if municipality =="Teocuitatlán de Corona"
replace uniqueid=14093 if municipality =="Tepatitlán de Morelos"
replace uniqueid=14094 if municipality =="Tequila"
replace uniqueid=14095 if municipality =="Teuchitlan"
replace uniqueid=14096 if municipality =="Tizapán el Alto"
replace uniqueid=14097 if municipality =="Tlajomulco de Zúñiga"
replace uniqueid=14098 if municipality =="Tlaquepaque"
replace uniqueid=14099 if municipality =="Tolimán"
replace uniqueid=14100 if municipality =="Tomatlán"
replace uniqueid=14101 if municipality =="Tonalá"
replace uniqueid=14102 if municipality =="Tonaya"
replace uniqueid=14103 if municipality =="Tonila"
replace uniqueid=14104 if municipality =="Totatiche"
replace uniqueid=14105 if municipality =="Tototlán"
replace uniqueid=14106 if municipality =="Tuxcacuesco"
replace uniqueid=14107 if municipality =="Tuxcueca"
replace uniqueid=14108 if municipality =="Tuxpan"
replace uniqueid=14109 if municipality =="Unión de San Antonio"
replace uniqueid=14110 if municipality =="Unión de Tula"
replace uniqueid=14111 if municipality =="Valle de Guadalupe"
replace uniqueid=14112 if municipality =="Valle de Juárez"
replace uniqueid=14114 if municipality =="Villa Corona"
replace uniqueid=14115 if municipality =="Villa Guerrero"
replace uniqueid=14116 if municipality =="Villa Hidalgo"
replace uniqueid=14068 if municipality =="Villa Purificación"
replace uniqueid=14118 if municipality =="Yahualica de González Gallo"
replace uniqueid=14119 if municipality =="Zacoalco de Torres"
replace uniqueid=14120 if municipality =="Zapopan"
replace uniqueid=14121 if municipality =="Zapotiltic"
replace uniqueid=14122 if municipality =="Zapotitlán de Vadillo"
replace uniqueid=14123 if municipality =="Zapotlán del Rey"
replace uniqueid=14023 if municipality =="Ciudad Guzmán"
replace uniqueid=14124 if municipality =="Zapotlanejo"

egen valid = rowtotal(PAN PRI PRD Partido_Cardenista PT PVEM PPS PDM)

foreach var in PAN PRI PRD Partido_Cardenista PT PVEM PPS PDM total  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_Partido_Cardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PDM, gen(PAN_r PRI_r PRD_r Partido_Cardenista_r PT_r PVEM_r PPS_r PDM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "Partido_Cardenista" if Partido_Cardenista_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PDM" if PDM_r==1
drop *_r

gen year = 1997
gen month ="November"

sort section

save Jalisco_Section_1997.dta, replace

keep municipality section uniqueid
bys section: gen x=_n
keep if x==1
drop x
sort section 

save Jalisco_Section_1997_for_Merge.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1995.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality==. & section==.
drop votaron

egen total = rowtotal(pan pri pps prd pfcrn parm pdm pt pvem ppj nulos)
drop if total==. | total==0

destring listanominal -  total, replace

collapse (sum) listanominal -  total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pfcrn  Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM
rename  parm   PARM
rename  pps    PPS
rename  pdm    PDM
rename  ppj    PPJ

gen turnout =  total/listanominal
drop  nulos  

egen valid = rowtotal(PAN PRI PPS PRD Partido_Cardenista PARM PDM PT PVEM PPJ)

foreach var in PAN PRI PPS PRD Partido_Cardenista PARM PDM PT PVEM PPJ listanominal total valid {
bys municipality: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_Partido_Cardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PDM inv_mun_PARM inv_mun_PPJ, gen(PAN_r PRI_r PRD_r Partido_Cardenista_r PT_r PVEM_r PPS_r PDM_r PARM_r PPJ_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "Partido_Cardenista" if Partido_Cardenista_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PDM" if PDM_r==1
replace winner = "PARM" if PARM_r==1
replace winner = "PPJ" if PPJ_r==1
drop *_r

gen year = 1995
gen month ="February"

drop municipality

sort section
merge section using Jalisco_Section_1997_for_Merge.dta
drop if _merge==2
drop _merge
replace uniqueid = 14089 if uniqueid ==. & section== 2300
replace uniqueid = 14092 if uniqueid ==. & section== 3338

replace municipality = "Techaluta de Montenegro" if uniqueid ==14089 & section== 2300
replace municipality = "Teocuitatlán de Corona" if uniqueid ==14092 & section== 3338

drop turnout listanominal mun_turnout mun_listanominal

save Jalisco_Section_1995.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2000_No_LN.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality==. & section==.

*** Very weird cases ***
drop if municipality==99 & section==924
drop if municipality==99 & section==925
drop if municipality==99 & section==926
drop if municipality==99 & section==928
drop if municipality==99 & section==2465
drop if municipality==120 & section==2465
drop if municipality==99 & section==3172
drop if municipality==99 & section==3174
drop if municipality==99 & section==3199
drop if municipality==99 & section==3206
drop if municipality==99 & section==3223
drop if municipality==99 & section==3225

egen total = rowtotal(pan pri prd pt pvem convergencia pcd psn parm pas ds)
drop if total==. | total==0

destring pan -   total, replace

collapse (sum) pan -  total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  convergencia     PC
rename  pt     PT
rename  pvem   PVEM
rename  pcd    PCD
rename  parm   PARM
rename  pas    PAS
* Democracia Social
rename  ds     PDS
rename  psn    PSN

* gen turnout =  total/listanominal

drop    noregistrados nulos 
egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)

foreach var in PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS total valid {
bys municipality: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCD inv_mun_PSN inv_mun_PARM inv_mun_PAS inv_mun_PDS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PCD_r PSN_r PARM_r PAS_r PDS_r)
drop inv_mun_*
     
gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PCD" if PCD_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PARM" if PARM_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PDS" if PDS_r==1
drop *_r

gen year = 2000
gen month ="November"

drop municipality 

sort section
merge section using Jalisco_Section_1997_for_Merge.dta
drop if _merge==2
drop _merge
replace uniqueid = 14089 if uniqueid ==. & section== 2300
replace uniqueid = 14098 if uniqueid ==. & section== 3310
replace uniqueid = 14101 if uniqueid ==. & section== 3311
replace municipality = "Techaluta de Montenegro" if uniqueid ==14089 & section== 2300
replace municipality = "Tlaquepaque" if uniqueid ==14098 & section== 3310
replace municipality = "Tonalá" if uniqueid ==14101 & section== 3311

save Jalisco_Section_2000.dta, replace

use Jalisco_Section_1997.dta, clear
drop if total==0

save Jalisco_Section_1997.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2003_No_LN.csv, clear
destring *, replace
save Ayu_Seccion_2003_No_LN.dta, replace
insheet using Ayu_Seccion_2003_No_LN_Extra.csv, clear
save Ayu_Seccion_2003_No_LN_Extra.dta, replace

use Ayu_Seccion_2003_No_LN.dta, clear
append using Ayu_Seccion_2003_No_LN_Extra.dta 

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
egen total =  rowtotal(pan  pri prd pt pvem pc psn pas elbarzon mexicoposible plm fc prdpas prdpc prdpvem pvempas prdpvempas)
drop if total==. | total==0

collapse (sum) pan - fc prdpas -total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM
rename  psn    PSN
rename  pas    PAS
rename  elbarzon    ElBarzon
rename  mexicoposible    MexicoPosible
rename  plm    PLM
rename  fc     FC
rename  prdpas     PRD_PAS
rename  prdpc     PRD_Partido_Cardenista
rename  prdpvem     PRD_PVEM
rename  pvempas     PVEM_PAS
rename  prdpvempas     PRD_PVEM_PAS

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=14001 if municipality =="ACATIC"
replace uniqueid=14002 if municipality =="ACATLAN DE JUAREZ"
replace uniqueid=14003 if municipality =="AHUALULCO DE MERCADO"
replace uniqueid=14004 if municipality =="AMACUECA"
replace uniqueid=14005 if municipality =="AMATITAN"
replace uniqueid=14006 if municipality =="AMECA"
replace uniqueid=14008 if municipality =="ARANDAS"
replace uniqueid=14010 if municipality =="ATEMAJAC DE BRIZUELA"
replace uniqueid=14011 if municipality =="ATENGO"
replace uniqueid=14012 if municipality =="ATENGUILLO"
replace uniqueid=14013 if municipality =="ATOTONILCO EL ALTO"
replace uniqueid=14014 if municipality =="ATOYAC"
replace uniqueid=14015 if municipality =="AUTLAN DE NAVARRO"
replace uniqueid=14016 if municipality =="AYOTLAN"
replace uniqueid=14017 if municipality =="AYUTLA"
replace uniqueid=14019 if municipality =="BOLANOS"
replace uniqueid=14020 if municipality =="CABO CORRIENTES"
replace uniqueid=14117 if municipality =="CANADAS DE OBREGON"
replace uniqueid=14021 if municipality =="CASIMIRO CASTILLO"
replace uniqueid=14030 if municipality =="CHAPALA"
replace uniqueid=14031 if municipality =="CHIMALTITAN"
replace uniqueid=14032 if municipality =="CHIQUILISTLAN"
replace uniqueid=14022 if municipality =="CIHUATLAN"
replace uniqueid=14024 if municipality =="COCULA"
replace uniqueid=14025 if municipality =="COLOTLAN"
replace uniqueid=14026 if municipality =="CONCEPCION DE BUENOS AIRES"
replace uniqueid=14027 if municipality =="CUAUTITLAN DE GARCIA BARRAGAI"
replace uniqueid=14028 if municipality =="CUAUTLA"
replace uniqueid=14029 if municipality =="CUQUIO"
replace uniqueid=14033 if municipality =="DEGOLLADO"
replace uniqueid=14034 if municipality =="EJUTLA"
replace uniqueid=14009 if municipality =="EL ARENAL"
replace uniqueid=14037 if municipality =="EL GRULLO"
replace uniqueid=14054 if municipality =="EL LIMON"
replace uniqueid=14070 if municipality =="EL SALTO"
replace uniqueid=14035 if municipality =="ENCARNACION DE DIAZ"
replace uniqueid=14036 if municipality =="ETZATLAN"
replace uniqueid=14079 if municipality =="GOMEZ FARIAS"
replace uniqueid=14038 if municipality =="GUACHINANGO"
replace uniqueid=14039 if municipality =="GUADALAJARA"
replace uniqueid=14040 if municipality =="HOSTOTIPAQUILLO"
replace uniqueid=14041 if municipality =="HUEJUCAR"
replace uniqueid=14042 if municipality =="HUEJUQUILLA EL ALTO"
replace uniqueid=14044 if municipality =="IXTLAHUACAN DE LOS MEMBRILLO"
replace uniqueid=14045 if municipality =="IXTLAHUACAN DEL RIO"
replace uniqueid=14046 if municipality =="JALOSTOTITLAN"
replace uniqueid=14047 if municipality =="JAMAY"
replace uniqueid=14048 if municipality =="JESUS MARIA"
replace uniqueid=14049 if municipality =="JILOTLAN DE LOS DOLORES"
replace uniqueid=14050 if municipality =="JOCOTEPEC"
replace uniqueid=14051 if municipality =="JUANACATLAN"
replace uniqueid=14052 if municipality =="JUCHITLAN"
replace uniqueid=14018 if municipality =="LA BARCA"
replace uniqueid=14043 if municipality =="LA HUERTA"
replace uniqueid=14057 if municipality =="MANZANILLA DE LA PAZ"
replace uniqueid=14053 if municipality =="LAGOS DE MORENO"
replace uniqueid=14055 if municipality =="MAGDALENA"
replace uniqueid=14058 if municipality =="MASCOTA"
replace uniqueid=14059 if municipality =="MAZAMITLA"
replace uniqueid=14060 if municipality =="MEXTICACAN"
replace uniqueid=14061 if municipality =="MEZQUITIC"
replace uniqueid=14062 if municipality =="MIXTLAN"
replace uniqueid=14063 if municipality =="OCOTLAN"
replace uniqueid=14064 if municipality =="OJUELOS DE JALISCO"
replace uniqueid=14065 if municipality =="PIHUAMO"
replace uniqueid=14066 if municipality =="PONCITLAN"
replace uniqueid=14067 if municipality =="PUERTO VALLARTA"
replace uniqueid=14069 if municipality =="QUITUPAN"
replace uniqueid=14071 if municipality =="SAN CRISTOBAL DE LA BARRANCA"
replace uniqueid=14072 if municipality =="SAN DIEGO DE ALEJANDRIA"
replace uniqueid=14113 if municipality =="SAN GABRIEL"
replace uniqueid=14073 if municipality =="SAN JUAN DE LOS LAGOS"
replace uniqueid=14007 if municipality =="SAN JUANITO DE ESCOBEDO"
replace uniqueid=14074 if municipality =="SAN JULIAN"
replace uniqueid=14075 if municipality =="SAN MARCOS"
replace uniqueid=14076 if municipality =="SAN MARTIN DE BOLANOS"
replace uniqueid=14077 if municipality =="SAN MARTIN HIDALGO"
replace uniqueid=14078 if municipality =="SAN MIGUEL EL ALTO"
replace uniqueid=14080 if municipality =="SAN SEBASTIAN DEL OESTE"
replace uniqueid=14081 if municipality =="SANTA MARIA DE LOS ANGELES"
replace uniqueid=14056 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=14082 if municipality =="SAYULA"
replace uniqueid=14083 if municipality =="TALA"
replace uniqueid=14084 if municipality =="TALPA DE ALLENDE"
replace uniqueid=14085 if municipality =="TAMAZULA DE GORDIANO"
replace uniqueid=14086 if municipality =="TAPALPA"
replace uniqueid=14087 if municipality =="TECALITLAN"
replace uniqueid=14089 if municipality =="TECHALUTA DE MONTENEGRO"
replace uniqueid=14088 if municipality =="TECOLOTLAN"
replace uniqueid=14090 if municipality =="TENAMAXTLAN"
replace uniqueid=14091 if municipality =="TEOCALTICHE"
replace uniqueid=14092 if municipality =="TEOCUITATLAN DE CORONA"
replace uniqueid=14093 if municipality =="TEPATITLAN DE MORELOS"
replace uniqueid=14094 if municipality =="TEQUILA"
replace uniqueid=14095 if municipality =="TEUCHITLAN"
replace uniqueid=14096 if municipality =="TIZAPAN EL ALTO"
replace uniqueid=14097 if municipality =="TLAJOMULCO DE ZUNIGA"
replace uniqueid=14098 if municipality =="TLAQUEPAQUE"
replace uniqueid=14099 if municipality =="TOLIMAN"
replace uniqueid=14100 if municipality =="TOMATLAN"
replace uniqueid=14101 if municipality =="TONALA"
replace uniqueid=14102 if municipality =="TONAYA"
replace uniqueid=14103 if municipality =="TONILA"
replace uniqueid=14104 if municipality =="TOTATICHE"
replace uniqueid=14105 if municipality =="TOTOTLAN"
replace uniqueid=14106 if municipality =="TUXCACUESCO"
replace uniqueid=14107 if municipality =="TUXCUECA"
replace uniqueid=14108 if municipality =="TUXPAN"
replace uniqueid=14109 if municipality =="UNION DE SAN ANTONIO"
replace uniqueid=14110 if municipality =="UNION DE TULA"
replace uniqueid=14111 if municipality =="VALLE DE GUADALUPE"
replace uniqueid=14112 if municipality =="VALLE DE JUAREZ"
replace uniqueid=14114 if municipality =="VILLA CORONA"
replace uniqueid=14115 if municipality =="VILLA GUERRERO"
replace uniqueid=14116 if municipality =="VILLA HIDALGO"
replace uniqueid=14068 if municipality =="VILLA PURIFICACION"
replace uniqueid=14118 if municipality =="YAHUALICA DE GONZALEZ GALLO"
replace uniqueid=14119 if municipality =="ZACOALCO DE TORRES"
replace uniqueid=14120 if municipality =="ZAPOPAN"
replace uniqueid=14121 if municipality =="ZAPOTILTIC"
replace uniqueid=14122 if municipality =="ZAPOTITLAN DE VADILLO"
replace uniqueid=14123 if municipality =="ZAPOTLAN DEL REY"
replace uniqueid=14023 if municipality =="ZAPOTLAN EL GRANDE"
replace uniqueid=14124 if municipality =="ZAPOTLANEJO"

egen valid = rowtotal(PAN PRI PRD PT PVEM Partido_Cardenista PSN PAS ElBarzon MexicoPosible PLM FC PRD_PAS PRD_Partido_Cardenista PRD_PVEM PVEM_PAS PRD_PVEM_PAS)

foreach var in PAN PRI PRD PT PVEM Partido_Cardenista PSN PAS ElBarzon MexicoPosible PLM FC PRD_PAS PRD_Partido_Cardenista PRD_PVEM PVEM_PAS PRD_PVEM_PAS total  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_Partido_Cardenista inv_mun_PSN inv_mun_PAS inv_mun_ElBarzon inv_mun_MexicoPosible inv_mun_PLM inv_mun_FC inv_mun_PRD_PAS inv_mun_PRD_Partido_Cardenista inv_mun_PRD_PVEM inv_mun_PVEM_PAS inv_mun_PRD_PVEM_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r Partido_Cardenista_r PSN_r PAS_r ElBarzon_r MexicoPosible_r PLM_r FC_r PRD_PAS_r PRD_Partido_Cardenista_r PRD_PVEM_r PVEM_PAS_r PRD_PVEM_PAS_r)
drop inv_mun_*
       
gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "Partido_Cardenista" if Partido_Cardenista_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "ElBarzon" if ElBarzon_r==1
replace winner = "MexicoPosible" if MexicoPosible_r==1
replace winner = "PLM" if PLM_r==1
replace winner = "FC" if FC_r==1
replace winner = "PRD_PAS" if PRD_PAS_r==1
replace winner = "PRD_Partido_Cardenista" if PRD_Partido_Cardenista_r==1
replace winner = "PRD_PVEM" if PRD_PVEM_r==1
replace winner = "PVEM_PAS" if PVEM_PAS_r==1
replace winner = "PRD_PVEM_PAS " if PRD_PVEM_PAS_r==1
drop *_r

gen year = 2003
gen month ="July"

save Jalisco_Section_2003.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2006_No_LN.csv, clear

rename municipio  municipality
rename casillas section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0

destring pan - total, replace
collapse (sum) pan - pas total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prdpt    PRD_PT
rename  pc     PC
rename  pvem   PVEM
rename  panal    PANAL
rename  pas    PAS

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=14001 if municipality =="ACATIC"
replace uniqueid=14002 if municipality =="ACATLÁN DE JUÁREZ"
replace uniqueid=14003 if municipality =="AHUALULCO DE MERCADO"
replace uniqueid=14004 if municipality =="AMACUECA"
replace uniqueid=14005 if municipality =="AMATITÁN"
replace uniqueid=14006 if municipality =="AMECA"
replace uniqueid=14008 if municipality =="ARANDAS"
replace uniqueid=14010 if municipality =="ATEMAJAC DE BRIZUELA"
replace uniqueid=14011 if municipality =="ATENGO"
replace uniqueid=14012 if municipality =="ATENGUILLO"
replace uniqueid=14013 if municipality =="ATOTONILCO EL ALTO"
replace uniqueid=14014 if municipality =="ATOYAC"
replace uniqueid=14015 if municipality =="AUTLÁN DE NAVARRO"
replace uniqueid=14016 if municipality =="AYOTLÁN"
replace uniqueid=14017 if municipality =="AYUTLA"
replace uniqueid=14019 if municipality =="BOLAÑOS"
replace uniqueid=14020 if municipality =="CABO CORRIENTE"
replace uniqueid=14117 if municipality =="CAÑADAS DE OBREGÓN"
replace uniqueid=14021 if municipality =="CASIMIRO CASTILLO"
replace uniqueid=14030 if municipality =="CHAPALA"
replace uniqueid=14031 if municipality =="CHIMALTITÁN"
replace uniqueid=14032 if municipality =="CHIQUILISTLÁN"
replace uniqueid=14022 if municipality =="CIHUATLAN"
replace uniqueid=14024 if municipality =="COCULA"
replace uniqueid=14025 if municipality =="COLOTLÁN"
replace uniqueid=14026 if municipality =="CONCEPCIÓN DE BUENOS AIRES"
replace uniqueid=14027 if municipality =="CUAUTITLÁN DE GARCÍA BARRAGÁN"
replace uniqueid=14028 if municipality =="CUAUTLA"
replace uniqueid=14029 if municipality =="CUQUÍO"
replace uniqueid=14033 if municipality =="DEGOLLADO"
replace uniqueid=14034 if municipality =="EJUTLA"
replace uniqueid=14009 if municipality =="EL ARENAL"
replace uniqueid=14037 if municipality =="EL GRULLO"
replace uniqueid=14054 if municipality =="EL LIMÓN"
replace uniqueid=14070 if municipality =="EL SALTO"
replace uniqueid=14035 if municipality =="ENCARNACIÓN DE DÍAZ"
replace uniqueid=14036 if municipality =="ETZATLÁN"
replace uniqueid=14079 if municipality =="GÓMEZ FARÍAS"
replace uniqueid=14038 if municipality =="GUACHINANGO"
replace uniqueid=14039 if municipality =="GUADALAJARA"
replace uniqueid=14040 if municipality =="HOSTOTIPAQUILLO"
replace uniqueid=14041 if municipality =="HUEJÚCAR"
replace uniqueid=14042 if municipality =="HUEJUQUILLA EL ALTO"
replace uniqueid=14044 if municipality =="IXTLAHUACÁN DE LOS MEMBRILLOS"
replace uniqueid=14045 if municipality =="IXTLAHUACÁN DEL RIO"
replace uniqueid=14046 if municipality =="JALOSTOTITLÁN"
replace uniqueid=14047 if municipality =="JAMAY"
replace uniqueid=14048 if municipality =="JESÚS MARÍA"
replace uniqueid=14049 if municipality =="JILOTLÁN DE LOS DOLORES"
replace uniqueid=14050 if municipality =="JOCOTEPEC"
replace uniqueid=14051 if municipality =="JUANACATLÁN"
replace uniqueid=14052 if municipality =="JUCHITLAN"
replace uniqueid=14018 if municipality =="LA BARCA"
replace uniqueid=14043 if municipality =="LA HUERTA"
replace uniqueid=14057 if municipality =="LA MANZANILLA DE LA PAZ"
replace uniqueid=14053 if municipality =="LAGOS DE MORENO"
replace uniqueid=14055 if municipality =="MAGDALENA"
replace uniqueid=14058 if municipality =="MASCOTA"
replace uniqueid=14059 if municipality =="MAZAMITLA"
replace uniqueid=14060 if municipality =="MEXTICACÁN"
replace uniqueid=14061 if municipality =="MEZQUITIC"
replace uniqueid=14062 if municipality =="MIXTLÁN"
replace uniqueid=14063 if municipality =="OCOTLÁN"
replace uniqueid=14064 if municipality =="OJUELOS DE JALISCO"
replace uniqueid=14065 if municipality =="PIHUAMO"
replace uniqueid=14066 if municipality =="PONCITLÁN"
replace uniqueid=14067 if municipality =="PUERTO VALLARTA"
replace uniqueid=14069 if municipality =="QUITUPAN"
replace uniqueid=14071 if municipality =="SAN CRISTÓBAL DE LA BARRANCA"
replace uniqueid=14072 if municipality =="SAN DIEGO DE ALEJANDRÍA"
replace uniqueid=14113 if municipality =="SAN GABRIEL"
replace uniqueid=14125 if municipality =="SAN IGNACIO CERRO GORDO"
replace uniqueid=14073 if municipality =="SAN JUAN DE LOS LAGOS"
replace uniqueid=14007 if municipality =="SAN JUANITO DE ESCOBEDO"
replace uniqueid=14074 if municipality =="SAN JULIÁN"
replace uniqueid=14075 if municipality =="SAN MARCOS"
replace uniqueid=14076 if municipality =="SAN MARTÍN DE BOLAÑOS"
replace uniqueid=14077 if municipality =="SAN MARTÍN HIDALGO"
replace uniqueid=14078 if municipality =="SAN MIGUEL EL ALTO"
replace uniqueid=14080 if municipality =="SAN SEBASTIÁN DEL OESTE"
replace uniqueid=14081 if municipality =="SANTA MARÍA DE LOS ÁNGELES"
replace uniqueid=14056 if municipality =="SANTA MARÍA DEL ORO"
replace uniqueid=14082 if municipality =="SAYULA"
replace uniqueid=14083 if municipality =="TALA"
replace uniqueid=14084 if municipality =="TALPA DE ALLENDE"
replace uniqueid=14085 if municipality =="TAMAZULA DE GORDIANO"
replace uniqueid=14086 if municipality =="TAPALPA"
replace uniqueid=14087 if municipality =="TECALITLÁN"
replace uniqueid=14089 if municipality =="TECHALUTA DE MONTENEGRO"
replace uniqueid=14088 if municipality =="TECOLOTLÁN"
replace uniqueid=14090 if municipality =="TENAMAXTLÁN"
replace uniqueid=14091 if municipality =="TEOCALTICHE"
replace uniqueid=14092 if municipality =="TEOCUITATLÁN DE CORONA"
replace uniqueid=14093 if municipality =="TEPATITLÁN DE MORELOS"
replace uniqueid=14094 if municipality =="TEQUILA"
replace uniqueid=14095 if municipality =="TEUCHITLÁN"
replace uniqueid=14096 if municipality =="TIZAPÁN EL ALTO"
replace uniqueid=14097 if municipality =="TLAJOMULCO DE ZÚÑIGA"
replace uniqueid=14098 if municipality =="TLAQUEPAQUE"
replace uniqueid=14099 if municipality =="TOLIMÁN"
replace uniqueid=14100 if municipality =="TOMATLÁN"
replace uniqueid=14101 if municipality =="TONALÁ"
replace uniqueid=14102 if municipality =="TONAYA"
replace uniqueid=14103 if municipality =="TONILA"
replace uniqueid=14104 if municipality =="TOTATICHE"
replace uniqueid=14105 if municipality =="TOTOTLÁN"
replace uniqueid=14106 if municipality =="TUXCACUESCO"
replace uniqueid=14107 if municipality =="TUXCUECA"
replace uniqueid=14108 if municipality =="TUXPAN"
replace uniqueid=14109 if municipality =="UNIÓN DE SAN ANTONIO"
replace uniqueid=14110 if municipality =="UNIÓN DE TULA"
replace uniqueid=14111 if municipality =="VALLE DE GUADALUPE"
replace uniqueid=14112 if municipality =="VALLE DE JUÁREZ"
replace uniqueid=14114 if municipality =="VILLA CORONA"
replace uniqueid=14115 if municipality =="VILLA GUERRERO"
replace uniqueid=14116 if municipality =="VILLA HIDALGO"
replace uniqueid=14068 if municipality =="VILLA PURIFICACIÓN"
replace uniqueid=14118 if municipality =="YAHUALICA DE GONZÁLEZ GALLO"
replace uniqueid=14119 if municipality =="ZACOALCO DE TORRES"
replace uniqueid=14120 if municipality =="ZAPOPAN"
replace uniqueid=14121 if municipality =="ZAPOTILTIC"
replace uniqueid=14122 if municipality =="ZAPOTITLÁN DE VADILLO"
replace uniqueid=14123 if municipality =="ZAPOTLÁN DEL REY"
replace uniqueid=14023 if municipality =="ZAPOTLÁN EL GRANDE"
replace uniqueid=14124 if municipality =="ZAPOTLANEJO"


egen valid = rowtotal(PAN PRI PRD_PT PVEM PC PANAL PAS)

foreach var in PAN PRI PRD_PT PVEM PC PANAL PAS total  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAS, gen(PAN_r PRI_r PRD_PT_r PVEM_r PC_r PANAL_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year = 2006
gen month ="July"

sort section

save Jalisco_Section_2006.dta, replace


*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2009_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if total==. | total==0

gen dummy_pripanal = (totalpripanal!=.)
gen dummy_prdpt = (totalprdpt!=.)
gen dummy_ptpc = (totalptconvergencia!=.)

drop if municipality=="" & section==.

destring pan - dummy_ptpc, replace

collapse (sum) pan - totalptconvergencia total - dummy_ptpc, by (municipality section)

replace dummy_pripanal = 1 if dummy_pripanal>0
replace dummy_prdpt = 1 if dummy_prdpt>0
replace dummy_ptpc = 1 if dummy_ptpc>0

************************************************************************************

replace pri   = 0 if dummy_pripanal == 1
replace panal = 0 if dummy_pripanal == 1
replace pripanal = 0 if dummy_pripanal == 1 
sum pri panal pripanal
drop pri panal pripanal
rename  totalpripanal  pripanal

replace prd  =0 if dummy_prdpt == 1
replace pt = 0 if dummy_prdpt == 1
replace prdpt =0 if dummy_prdpt == 1 
sum prdpt
drop prdpt 
rename totalprdpt prdpt

replace pt = 0 if dummy_ptpc == 1
replace convergencia = 0 if dummy_ptpc == 1
replace ptconvergencia = 0 if dummy_ptpc == 1
sum ptconvergencia
drop ptconvergencia
rename  totalptconvergencia ptpc

drop dummy_pripanal dummy_prdpt dummy_ptpc

************************************************************************************

rename  pan    PAN
rename  pripanal    PRI_PANAL
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM
rename  convergencia     PC
rename  psd    PSD
rename  prdpt  PRD_PT
rename  ptpc   PT_PC
 
* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=14001 if municipality =="ACATIC"
replace uniqueid=14002 if municipality =="ACATLÁN DE JUÁREZ"
replace uniqueid=14003 if municipality =="AHUALULCO DE MERCADO"
replace uniqueid=14004 if municipality =="AMACUECA"
replace uniqueid=14005 if municipality =="AMATITÁN"
replace uniqueid=14006 if municipality =="AMECA"
replace uniqueid=14008 if municipality =="ARANDAS"
replace uniqueid=14010 if municipality =="ATEMAJAC DE BRIZUELA"
replace uniqueid=14011 if municipality =="ATENGO"
replace uniqueid=14012 if municipality =="ATENGUILLO"
replace uniqueid=14013 if municipality =="ATOTONILCO EL ALTO"
replace uniqueid=14014 if municipality =="ATOYAC"
replace uniqueid=14015 if municipality =="AUTLÁN DE NAVARRO"
replace uniqueid=14016 if municipality =="AYOTLÁN"
replace uniqueid=14017 if municipality =="AYUTLA"
replace uniqueid=14019 if municipality =="BOLAÑOS"
replace uniqueid=14020 if municipality =="CABO CORRIENTES"
replace uniqueid=14117 if municipality =="CAÑADAS DE OBREGÓN"
replace uniqueid=14021 if municipality =="CASIMIRO CASTILLO"
replace uniqueid=14030 if municipality =="CHAPALA"
replace uniqueid=14031 if municipality =="CHIMALTITÁN"
replace uniqueid=14032 if municipality =="CHIQUILISTLÁN"
replace uniqueid=14022 if municipality =="CIHUATLÁN"
replace uniqueid=14024 if municipality =="COCULA"
replace uniqueid=14025 if municipality =="COLOTLÁN"
replace uniqueid=14026 if municipality =="CONCEPCIÓN DE BUENOS AIRES"
replace uniqueid=14027 if municipality =="CUAUTITLÁN DE GARCÍA BARRAGÁN"
replace uniqueid=14028 if municipality =="CUAUTLA"
replace uniqueid=14029 if municipality =="CUQUÍO"
replace uniqueid=14033 if municipality =="DEGOLLADO"
replace uniqueid=14034 if municipality =="EJUTLA"
replace uniqueid=14009 if municipality =="EL ARENAL"
replace uniqueid=14037 if municipality =="EL GRULLO"
replace uniqueid=14054 if municipality =="EL LIMÓN"
replace uniqueid=14070 if municipality =="EL SALTO"
replace uniqueid=14035 if municipality =="ENCARNACIÓN DE DÍAZ"
replace uniqueid=14036 if municipality =="ETZATLÁN"
replace uniqueid=14079 if municipality =="GÓMEZ FARÍAS"
replace uniqueid=14038 if municipality =="GUACHINANGO"
replace uniqueid=14039 if municipality =="GUADALAJARA"
replace uniqueid=14040 if municipality =="HOSTOTIPAQUILLO"
replace uniqueid=14041 if municipality =="HUEJÚCAR"
replace uniqueid=14042 if municipality =="HUEJUQUILLA EL ALTO"
replace uniqueid=14044 if municipality =="IXTLAHUACÁN DE LOS MEMBRILLOS"
replace uniqueid=14045 if municipality =="IXTLAHUACÁN DEL RÍO"
replace uniqueid=14046 if municipality =="JALOSTOTITLÁN"
replace uniqueid=14047 if municipality =="JAMAY"
replace uniqueid=14048 if municipality =="JESÚS MARÍA"
replace uniqueid=14049 if municipality =="JILOTLÁN DE LOS DOLORES"
replace uniqueid=14050 if municipality =="JOCOTEPEC"
replace uniqueid=14051 if municipality =="JUANACATLÁN"
replace uniqueid=14052 if municipality =="JUCHITLÁN"
replace uniqueid=14018 if municipality =="LA BARCA"
replace uniqueid=14043 if municipality =="LA HUERTA"
replace uniqueid=14057 if municipality =="MANZANILLA DE LA PAZ"
replace uniqueid=14053 if municipality =="LAGOS DE MORENO"
replace uniqueid=14055 if municipality =="MAGDALENA"
replace uniqueid=14058 if municipality =="MASCOTA"
replace uniqueid=14059 if municipality =="MAZAMITLA"
replace uniqueid=14060 if municipality =="MEXTICACÁN"
replace uniqueid=14061 if municipality =="MEZQUITIC"
replace uniqueid=14062 if municipality =="MIXTLÁN"
replace uniqueid=14063 if municipality =="OCOTLÁN"
replace uniqueid=14064 if municipality =="OJUELOS DE JALISCO"
replace uniqueid=14065 if municipality =="PIHUAMO"
replace uniqueid=14066 if municipality =="PONCITLÁN"
replace uniqueid=14067 if municipality =="PUERTO VALLARTA"
replace uniqueid=14069 if municipality =="QUITUPAN"
replace uniqueid=14071 if municipality =="SAN CRISTÓBAL DE LA BARRANCA"
replace uniqueid=14072 if municipality =="SAN DIEGO DE ALEJANDRÍA"
replace uniqueid=14113 if municipality =="SAN GABRIEL"
replace uniqueid=14125 if municipality =="SAN IGNACIO CERRO GORDO"
replace uniqueid=14073 if municipality =="SAN JUAN DE LOS LAGOS"
replace uniqueid=14007 if municipality =="SAN JUANITO DE ESCOBEDO"
replace uniqueid=14074 if municipality =="SAN JULIÁN"
replace uniqueid=14075 if municipality =="SAN MARCOS"
replace uniqueid=14076 if municipality =="SAN MARTÍN DE BOLAÑOS"
replace uniqueid=14077 if municipality =="SAN MARTÍN HIDALGO"
replace uniqueid=14078 if municipality =="SAN MIGUEL EL ALTO"
replace uniqueid=14080 if municipality =="SAN SEBASTIÁN DEL OESTE"
replace uniqueid=14081 if municipality =="SANTA MARÍA DE LOS ANGELES"
replace uniqueid=14056 if municipality =="SANTA MARÍA DEL ORO"
replace uniqueid=14082 if municipality =="SAYULA"
replace uniqueid=14083 if municipality =="TALA"
replace uniqueid=14084 if municipality =="TALPA DE ALLENDE"
replace uniqueid=14085 if municipality =="TAMAZULA DE GORDIANO"
replace uniqueid=14086 if municipality =="TAPALPA"
replace uniqueid=14087 if municipality =="TECALITLÁN"
replace uniqueid=14089 if municipality =="TECHALUTA DE MONTENEGRO"
replace uniqueid=14088 if municipality =="TECOLOTLÁN"
replace uniqueid=14090 if municipality =="TENAMAXTLÁN"
replace uniqueid=14091 if municipality =="TEOCALTICHE"
replace uniqueid=14092 if municipality =="TEOCUITATLÁN DE CORONA"
replace uniqueid=14093 if municipality =="TEPATITLÁN DE MORELOS"
replace uniqueid=14094 if municipality =="TEQUILA"
replace uniqueid=14095 if municipality =="TEUCHITLÁN"
replace uniqueid=14096 if municipality =="TIZAPÁN EL ALTO"
replace uniqueid=14097 if municipality =="TLAJOMULCO DE ZÚÑIGA"
replace uniqueid=14098 if municipality =="TLAQUEPAQUE"
replace uniqueid=14099 if municipality =="TOLIMÁN"
replace uniqueid=14100 if municipality =="TOMATLÁN"
replace uniqueid=14101 if municipality =="TONALÁ"
replace uniqueid=14102 if municipality =="TONAYA"
replace uniqueid=14103 if municipality =="TONILA"
replace uniqueid=14104 if municipality =="TOTATICHE"
replace uniqueid=14105 if municipality =="TOTOTLÁN"
replace uniqueid=14106 if municipality =="TUXCACUESCO"
replace uniqueid=14107 if municipality =="TUXCUECA"
replace uniqueid=14108 if municipality =="TUXPAN"
replace uniqueid=14109 if municipality =="UNIÓN DE SAN ANTONIO"
replace uniqueid=14110 if municipality =="UNIÓN DE TULA"
replace uniqueid=14111 if municipality =="VALLE DE GUADALUPE"
replace uniqueid=14112 if municipality =="VALLE DE JUÁREZ"
replace uniqueid=14114 if municipality =="VILLA CORONA"
replace uniqueid=14115 if municipality =="VILLA GUERRERO"
replace uniqueid=14116 if municipality =="VILLA HIDALGO"
replace uniqueid=14068 if municipality =="VILLA PURIFICACIÓN"
replace uniqueid=14118 if municipality =="YAHUALICA DE GONZÁLEZ GALLO"
replace uniqueid=14119 if municipality =="ZACOALCO DE TORRES"
replace uniqueid=14120 if municipality =="ZAPOPAN"
replace uniqueid=14121 if municipality =="ZAPOTILTIC"
replace uniqueid=14122 if municipality =="ZAPOTITLÁN DE VADILLO"
replace uniqueid=14123 if municipality =="ZAPOTLÁN DEL REY"
replace uniqueid=14023 if municipality =="ZAPOTLÁN EL GRANDE"
replace uniqueid=14124 if municipality =="ZAPOTLANEJO"

egen valid = rowtotal(PAN PRD PT PVEM PC PSD PRI_PANAL PRD_PT PT_PC)

foreach var in PAN PRD PT PVEM PC PSD PRI_PANAL PRD_PT PT_PC total  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSD inv_mun_PRI_PANAL inv_mun_PRD_PT inv_mun_PT_PC, gen(PAN_r PRD_r PT_r PVEM_r PC_r PSD_r PRI_PANAL_r PRD_PT_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSD" if PSD_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year = 2009
gen month ="July"

sort section

save Jalisco_Section_2009.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2012.xlsx", sheet("CasillaXCasilla") firstrow clear

rename Municipio municipality
gen section = subinstr(Casilla, "B", "", .)
forvalues i = 1(1)9 {
replace section = subinstr(section, "C0`i'", "", .)
replace section = subinstr(section, "E0`i'", "", .)
}

forvalues i = 10(1)21 {
replace section = subinstr(section, "C`i'", "", .)
replace section = subinstr(section, "E`i'", "", .)
}

destring section, replace

gen test = COALPRIPVEM - PRI - PVEM - PRIPVEM + COALPTMC - PT - MC - PTMC
sum test
drop PRI PVEM PRIPVEM PT MC PTMC test
rename NAL PANAL
rename COALPRIPVEM PRI_PVEM 
rename COALPTMC PT_PC
rename Boletas listanominal
gen total = Validos + NULOS + NO_REGISTRADO

collapse (sum) PAN - listanominal total, by (municipality section)

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=14001 if municipality =="ACATIC"
replace uniqueid=14002 if municipality =="ACATLÁN DE JUÁREZ"
replace uniqueid=14003 if municipality =="AHUALULCO DE MERCADO"
replace uniqueid=14004 if municipality =="AMACUECA"
replace uniqueid=14005 if municipality =="AMATITÁN"
replace uniqueid=14006 if municipality =="AMECA"
replace uniqueid=14008 if municipality =="ARANDAS"
replace uniqueid=14010 if municipality =="ATEMAJAC DE BRIZUELA"
replace uniqueid=14011 if municipality =="ATENGO"
replace uniqueid=14012 if municipality =="ATENGUILLO"
replace uniqueid=14013 if municipality =="ATOTONILCO EL ALTO"
replace uniqueid=14014 if municipality =="ATOYAC"
replace uniqueid=14015 if municipality =="AUTLÁN DE NAVARRO"
replace uniqueid=14016 if municipality =="AYOTLÁN"
replace uniqueid=14017 if municipality =="AYUTLA"
replace uniqueid=14019 if municipality =="BOLAÑOS"
replace uniqueid=14020 if municipality =="CABO CORRIENTES"
replace uniqueid=14117 if municipality =="CAÑADAS DE OBREGÓN"
replace uniqueid=14021 if municipality =="CASIMIRO CASTILLO"
replace uniqueid=14030 if municipality =="CHAPALA"
replace uniqueid=14031 if municipality =="CHIMALTITÁN"
replace uniqueid=14032 if municipality =="CHIQUILISTLÁN"
replace uniqueid=14022 if municipality =="CIHUATLÁN"
replace uniqueid=14024 if municipality =="COCULA"
replace uniqueid=14025 if municipality =="COLOTLÁN"
replace uniqueid=14026 if municipality =="CONCEPCIÓN DE BUENOS AIRES"
replace uniqueid=14027 if municipality =="CUAUTITLÁN DE GARCÍA BARRAGÁN"
replace uniqueid=14028 if municipality =="CUAUTLA"
replace uniqueid=14029 if municipality =="CUQUÍO"
replace uniqueid=14033 if municipality =="DEGOLLADO"
replace uniqueid=14034 if municipality =="EJUTLA"
replace uniqueid=14009 if municipality =="EL ARENAL"
replace uniqueid=14037 if municipality =="EL GRULLO"
replace uniqueid=14054 if municipality =="EL LIMÓN"
replace uniqueid=14070 if municipality =="EL SALTO"
replace uniqueid=14035 if municipality =="ENCARNACIÓN DE DÍAZ"
replace uniqueid=14036 if municipality =="ETZATLÁN"
replace uniqueid=14079 if municipality =="GÓMEZ FARÍAS"
replace uniqueid=14038 if municipality =="GUACHINANGO"
replace uniqueid=14039 if municipality =="GUADALAJARA"
replace uniqueid=14040 if municipality =="HOSTOTIPAQUILLO"
replace uniqueid=14041 if municipality =="HUEJÚCAR"
replace uniqueid=14042 if municipality =="HUEJUQUILLA EL ALTO"
replace uniqueid=14044 if municipality =="IXTLAHUACÁN DE LOS MEMBRILLOS"
replace uniqueid=14045 if municipality =="IXTLAHUACÁN DEL RÍO"
replace uniqueid=14046 if municipality =="JALOSTOTITLÁN"
replace uniqueid=14047 if municipality =="JAMAY"
replace uniqueid=14048 if municipality =="JESÚS MARÍA"
replace uniqueid=14049 if municipality =="JILOTLÁN DE LOS DOLORES"
replace uniqueid=14050 if municipality =="JOCOTEPEC"
replace uniqueid=14051 if municipality =="JUANACATLÁN"
replace uniqueid=14052 if municipality =="JUCHITLÁN"
replace uniqueid=14018 if municipality =="LA BARCA"
replace uniqueid=14043 if municipality =="LA HUERTA"
replace uniqueid=14057 if municipality =="MANZANILLA DE LA PAZ"
replace uniqueid=14053 if municipality =="LAGOS DE MORENO"
replace uniqueid=14055 if municipality =="MAGDALENA"
replace uniqueid=14058 if municipality =="MASCOTA"
replace uniqueid=14059 if municipality =="MAZAMITLA"
replace uniqueid=14060 if municipality =="MEXTICACÁN"
replace uniqueid=14061 if municipality =="MEZQUITIC"
replace uniqueid=14062 if municipality =="MIXTLÁN"
replace uniqueid=14063 if municipality =="OCOTLÁN"
replace uniqueid=14064 if municipality =="OJUELOS DE JALISCO"
replace uniqueid=14065 if municipality =="PIHUAMO"
replace uniqueid=14066 if municipality =="PONCITLÁN"
replace uniqueid=14067 if municipality =="PUERTO VALLARTA"
replace uniqueid=14069 if municipality =="QUITUPAN"
replace uniqueid=14071 if municipality =="SAN CRISTÓBAL DE LA BARRANCA"
replace uniqueid=14072 if municipality =="SAN DIEGO DE ALEJANDRÍA"
replace uniqueid=14113 if municipality =="SAN GABRIEL"
replace uniqueid=14125 if municipality =="SAN IGNACIO CERRO GORDO"
replace uniqueid=14073 if municipality =="SAN JUAN DE LOS LAGOS"
replace uniqueid=14007 if municipality =="SAN JUANITO DE ESCOBEDO"
replace uniqueid=14074 if municipality =="SAN JULIÁN"
replace uniqueid=14075 if municipality =="SAN MARCOS"
replace uniqueid=14076 if municipality =="SAN MARTÍN DE BOLAÑOS"
replace uniqueid=14077 if municipality =="SAN MARTÍN HIDALGO"
replace uniqueid=14078 if municipality =="SAN MIGUEL EL ALTO"
replace uniqueid=14080 if municipality =="SAN SEBASTIÁN DEL OESTE"
replace uniqueid=14081 if municipality =="SANTA MARÍA DE LOS ANGELES"
replace uniqueid=14056 if municipality =="SANTA MARÍA DEL ORO"
replace uniqueid=14082 if municipality =="SAYULA"
replace uniqueid=14083 if municipality =="TALA"
replace uniqueid=14084 if municipality =="TALPA DE ALLENDE"
replace uniqueid=14085 if municipality =="TAMAZULA DE GORDIANO"
replace uniqueid=14086 if municipality =="TAPALPA"
replace uniqueid=14087 if municipality =="TECALITLÁN"
replace uniqueid=14089 if municipality =="TECHALUTA DE MONTENEGRO"
replace uniqueid=14088 if municipality =="TECOLOTLÁN"
replace uniqueid=14090 if municipality =="TENAMAXTLÁN"
replace uniqueid=14091 if municipality =="TEOCALTICHE"
replace uniqueid=14092 if municipality =="TEOCUITATLÁN DE CORONA"
replace uniqueid=14093 if municipality =="TEPATITLÁN DE MORELOS"
replace uniqueid=14094 if municipality =="TEQUILA"
replace uniqueid=14095 if municipality =="TEUCHITLÁN"
replace uniqueid=14096 if municipality =="TIZAPÁN EL ALTO"
replace uniqueid=14097 if municipality =="TLAJOMULCO DE ZÚÑIGA"
replace uniqueid=14098 if municipality =="SAN PEDRO TLAQUEPAQUE"
replace uniqueid=14099 if municipality =="TOLIMÁN"
replace uniqueid=14100 if municipality =="TOMATLÁN"
replace uniqueid=14101 if municipality =="TONALÁ"
replace uniqueid=14102 if municipality =="TONAYA"
replace uniqueid=14103 if municipality =="TONILA"
replace uniqueid=14104 if municipality =="TOTATICHE"
replace uniqueid=14105 if municipality =="TOTOTLÁN"
replace uniqueid=14106 if municipality =="TUXCACUESCO"
replace uniqueid=14107 if municipality =="TUXCUECA"
replace uniqueid=14108 if municipality =="TUXPAN"
replace uniqueid=14109 if municipality =="UNIÓN DE SAN ANTONIO"
replace uniqueid=14110 if municipality =="UNIÓN DE TULA"
replace uniqueid=14111 if municipality =="VALLE DE GUADALUPE"
replace uniqueid=14112 if municipality =="VALLE DE JUÁREZ"
replace uniqueid=14114 if municipality =="VILLA CORONA"
replace uniqueid=14115 if municipality =="VILLA GUERRERO"
replace uniqueid=14116 if municipality =="VILLA HIDALGO"
replace uniqueid=14068 if municipality =="VILLA PURIFICACIÓN"
replace uniqueid=14118 if municipality =="YAHUALICA DE GONZÁLEZ GALLO"
replace uniqueid=14119 if municipality =="ZACOALCO DE TORRES"
replace uniqueid=14120 if municipality =="ZAPOPAN"
replace uniqueid=14121 if municipality =="ZAPOTILTIC"
replace uniqueid=14122 if municipality =="ZAPOTITLÁN DE VADILLO"
replace uniqueid=14123 if municipality =="ZAPOTLÁN DEL REY"
replace uniqueid=14023 if municipality =="ZAPOTLÁN EL GRANDE"
replace uniqueid=14124 if municipality =="ZAPOTLANEJO"

egen valid = rowtotal(PAN PRD PANAL PRI_PVEM PT_PC )

foreach var in PAN PRD PANAL PRI_PVEM PT_PC  total listanominal  valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD inv_mun_PANAL inv_mun_PRI_PVEM inv_mun_PT_PC , gen(PAN_r PRD_r PANAL_r PRI_PVEM_r PT_PC_r )
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PANAL" if PANAL_r==1
replace winner = "PT_PC" if PT_PC_r==1
drop *_r

gen year = 2012
gen month ="July"

sort section

save Jalisco_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Jalisco_Section_1995.dta, clear
append using Jalisco_Section_1997.dta
append using Jalisco_Section_2000.dta
append using Jalisco_Section_2003.dta
append using Jalisco_Section_2006.dta
append using Jalisco_Section_2009.dta
append using Jalisco_Section_2012.dta

erase Jalisco_Section_1995.dta
erase Jalisco_Section_1997.dta
erase Jalisco_Section_2000.dta
erase Jalisco_Section_2003.dta
erase Jalisco_Section_2006.dta
erase Jalisco_Section_2009.dta
erase Jalisco_Section_2012.dta

tab winner, missing
br if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS Partido_Cardenista {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Jalisco_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Jalisco_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


