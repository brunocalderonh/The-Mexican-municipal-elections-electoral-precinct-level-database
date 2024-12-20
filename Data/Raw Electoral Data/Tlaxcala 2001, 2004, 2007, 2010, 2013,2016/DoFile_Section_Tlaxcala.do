
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Tlaxcala 2001, 2004, 2007, 2010, 2013"

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Listanominal2007.xlsx", sheet("datos") firstrow clear
sort section
save Listanominal2007.dta, replace

import excel "Listanominal2013.xlsx", sheet("datos") firstrow clear
sort section
save Listanominal2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001_No_LN.csv, clear
rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

destring pri - nulos , replace
egen total = rowtotal(pan pri  prd pt pvem partidodemocrata psn pc pas pcdt pjs  nulos)
drop if total==. | total==0 

collapse (sum)  pri - nulos total , by (municipality section)

*sort section 
*merge section using Ayu_Seccion_2001_LN.dta
*drop if _merge==2
*drop _merge

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
rename partidodemocrata Partido_Democrata
* PCDT stands for Partido Centro Democrático de Tlaxcala
rename pcdt PCDT
rename psn PSN
rename pas PAS
* PJS stands for Partido Justicia Social
rename pjs PJS

*gen turnout =  total/listanominal

drop  nulos   

gen   uniqueid= 0
replace uniqueid=29022 if municipality =="ACUAMANALA DE MIGUEL HIDALGO"
replace uniqueid=29001 if municipality =="AMAXAC DE GUERRERO"
replace uniqueid=29002 if municipality =="APETATITLAN DE ANTONIO CARVAJAL"
replace uniqueid=29005 if municipality =="APIZACO"
replace uniqueid=29003 if municipality =="ATLANGATEPEC"
replace uniqueid=29004 if municipality =="ALTZAYANCA"
replace uniqueid=29045 if municipality =="BENITO JUÁREZ"
replace uniqueid=29006 if municipality =="CALPULALPAN"
replace uniqueid=29010 if municipality =="CHIAUTEMPAN"
replace uniqueid=29018 if municipality =="CONTLA DE JUAN CUAMATZI"
replace uniqueid=29008 if municipality =="CUAPIAXTLA"
replace uniqueid=29009 if municipality =="CUAXOMULCO"
replace uniqueid=29007 if municipality =="EL CARMEN TEQUEXQUITLA"
replace uniqueid=29046 if municipality =="EMILIANO ZAPATA"
replace uniqueid=29012 if municipality =="ESPAÑITA"
replace uniqueid=29013 if municipality =="HUAMANTLA"
replace uniqueid=29014 if municipality =="HUEYOTLIPAN"
replace uniqueid=29015 if municipality =="IXTACUIXTLA DE MARIANO MATAMOROS"
replace uniqueid=29016 if municipality =="IXTENCO"
replace uniqueid=29048 if municipality =="LA MAGDALENA TLALTELULCO"
replace uniqueid=29047 if municipality =="LÁZARO CÁRDENAS"
replace uniqueid=29017 if municipality =="MAZATECOCHCO DE JOSÉ MARÍA MORELOS"
replace uniqueid=29011 if municipality =="MUNOZ DE DOMINGO ARENAS"
replace uniqueid=29021 if municipality =="NANACAMILPA DE MARIANO ARISTA"
replace uniqueid=29023 if municipality =="NATIVITAS"
replace uniqueid=29024 if municipality =="PANOTLA"
replace uniqueid=29041 if municipality =="PAPALOTLA DE XICOHTENCATL"
replace uniqueid=29049 if municipality =="SAN DAMIÁN TEXOLOC"
replace uniqueid=29050 if municipality =="SAN FRANCISCO TETLANOHCAN"
replace uniqueid=29051 if municipality =="SAN JERÓNIMO ZACUALPAN"
replace uniqueid=29052 if municipality =="SAN JOSÉ TEACALCO"
replace uniqueid=29053 if municipality =="SAN JUAN HUACTZINCO"
replace uniqueid=29054 if municipality =="SAN LORENZO AXOCOMANITLA"
replace uniqueid=29055 if municipality =="SAN LUCAS TECOPILCO"
replace uniqueid=29025 if municipality =="SAN PABLO DEL MONTE"
replace uniqueid=29020 if municipality =="SANCTORUM"
replace uniqueid=29056 if municipality =="SANTA ANA NOPALUCAN"
replace uniqueid=29057 if municipality =="SANTA APOLONIA TEACALCO"
replace uniqueid=29058 if municipality =="SANTA CATARINA AYOMETLA"
replace uniqueid=29059 if municipality =="SANTA CRUZ QUILEHTLA"
replace uniqueid=29026 if municipality =="SANTA CRUZ TLAXCALA"
replace uniqueid=29060 if municipality =="SANTA ISABEL XILOXOXTLA"
replace uniqueid=29027 if municipality =="TENANCINGO"
replace uniqueid=29028 if municipality =="SAN LUIS TEOLOCHOLCO"
replace uniqueid=29019 if municipality =="TEPETITLA DE LARDIZABAL"
replace uniqueid=29029 if municipality =="TEPEYANCO"
replace uniqueid=29030 if municipality =="TERRENATE"
replace uniqueid=29031 if municipality =="TETLA DE LA SOLIDARIDAD"
replace uniqueid=29032 if municipality =="TETLATLAHUCA"
replace uniqueid=29033 if municipality =="TLAXCALA"
replace uniqueid=29034 if municipality =="TLAXCO"
replace uniqueid=29035 if municipality =="TOCATLÁN"
replace uniqueid=29036 if municipality =="TOTOLAC"
replace uniqueid=29038 if municipality =="TZOMPANTEPEC"
replace uniqueid=29039 if municipality =="XALOZTOC"
replace uniqueid=29040 if municipality =="XALTOCAN"
replace uniqueid=29042 if municipality =="XICOHTZINCO"
replace uniqueid=29043 if municipality =="YAUHQUEMEHCAN"
replace uniqueid=29044 if municipality =="ZACATELCO"
replace uniqueid=29037 if municipality =="ZITLALTEPEC DE TRINIDAD SÁNCHEZ SANTOS"

egen valid = rowtotal(PRI PAN PRD PT PVEM Partido_Democrata PSN PC PAS PCDT PJS)

foreach var in PRI PAN PRD PT PVEM Partido_Democrata PSN PC PAS PCDT PJS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PAN inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_Partido_Democrata inv_mun_PSN inv_mun_PC inv_mun_PAS inv_mun_PCDT inv_mun_PJS, gen(PRI_r PAN_r PRD_r PT_r PVEM_r Partido_Democrata_r PSN_r PC_r PAS_r PCDT_r PJS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PCDT" if PCDT_r ==1 
replace winner = "PJS" if PJS_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PSN" if PSN_r ==1 
replace winner = "Partido_Democrata" if Partido_Democrata_r ==1 
drop *_r

gen year = 2001
gen month ="November"

sort section

save Tlaxcala_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


insheet using Ayu_Seccion_2004_LN.csv, clear
rename seccion section 
sort section 

save Ayu_Seccion_2004_LN.dta, replace

insheet using Ayu_Seccion_2004_No_LN.csv, clear

rename municipio  municipality
rename seccion section
rename emitidos total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan  - validos , replace

collapse (sum)  pan  - validos , by (municipality section)

sort section 
merge section using Ayu_Seccion_2004_LN.dta
drop if _merge==2
drop _merge

rename pan  PAN
rename pri  PRI
rename pripvem  PRI_PVEM
rename prd  PRD
rename pt   PT
rename ptpcdtpjs   PT_PCDT_PJS
rename ptpcdt  PT_PCDT
rename ptpjs   PT_PJS
rename pvem PVEM
rename pc   PC
rename pcdt PCDT
rename pcdtpjs PCDT_PJS
rename pjs PJS

gen turnout =  total/listanominal

drop  anulados validos  

gen   uniqueid= 0
replace uniqueid=29022 if municipality =="ACUAMANALA DE MIGUEL HIDALGO"
replace uniqueid=29001 if municipality =="AMAXAC DE GUERRERO"
replace uniqueid=29002 if municipality =="APETATITLAN DE ANTONIO CARVAJAL"
replace uniqueid=29005 if municipality =="APIZACO"
replace uniqueid=29003 if municipality =="ATLANGATEPEC"
replace uniqueid=29004 if municipality =="ATLTZAYANCA"
replace uniqueid=29045 if municipality =="BENITO JUAREZ"
replace uniqueid=29006 if municipality =="CALPULALPAN"
replace uniqueid=29010 if municipality =="CHIAUTEMPAN"
replace uniqueid=29018 if municipality =="CONTLA DE JUAN CUAMATZI"
replace uniqueid=29008 if municipality =="CUAPIAXTLA"
replace uniqueid=29009 if municipality =="CUAXOMULCO"
replace uniqueid=29007 if municipality =="EL CARMEN TEQUEXQUITLA"
replace uniqueid=29046 if municipality =="EMILIANO ZAPATA"
replace uniqueid=29012 if municipality =="ESPANITA"
replace uniqueid=29013 if municipality =="HUAMANTLA"
replace uniqueid=29014 if municipality =="HUEYOTLIPAN"
replace uniqueid=29015 if municipality =="IXTACUIXTLA DE MARIANO MATAMOROS"
replace uniqueid=29016 if municipality =="IXTENCO"
replace uniqueid=29048 if municipality =="LA MAGDALENA TLALTELULCO"
replace uniqueid=29047 if municipality =="LAZARO CARDENAS"
replace uniqueid=29017 if municipality =="MAZATECOCHCO DE JOSE MARIA MORELOS"
replace uniqueid=29011 if municipality =="MUNOZ DE DOMINGO ARENAS"
replace uniqueid=29021 if municipality =="NANACAMILPA DE MARIANO ARISTA"
replace uniqueid=29023 if municipality =="NATIVITAS"
replace uniqueid=29024 if municipality =="PANOTLA"
replace uniqueid=29041 if municipality =="PAPALOTLA DE XICOHTENCATL"
replace uniqueid=29049 if municipality =="SAN DAMIAN TEXOLOC"
replace uniqueid=29050 if municipality =="SAN FRANCISCO TETLANOHCAN"
replace uniqueid=29051 if municipality =="SAN JERONIMO ZACUALPAN"
replace uniqueid=29052 if municipality =="SAN JOSE TEACALCO"
replace uniqueid=29053 if municipality =="SAN JUAN HUACTZINCO"
replace uniqueid=29054 if municipality =="SAN LORENZO AXOCOMANITLA"
replace uniqueid=29055 if municipality =="SAN LUCAS TECOPILCO"
replace uniqueid=29025 if municipality =="SAN PABLO DEL MONTE"
replace uniqueid=29020 if municipality =="SANCTORUM"
replace uniqueid=29056 if municipality =="SANTA ANA NOPALUCAN"
replace uniqueid=29057 if municipality =="SANTA APOLONIA TEACALCO"
replace uniqueid=29058 if municipality =="SANTA CATARINA AYOMETLA"
replace uniqueid=29059 if municipality =="SANTA CRUZ QUILEHTLA"
replace uniqueid=29026 if municipality =="SANTA CRUZ TLAXCALA"
replace uniqueid=29060 if municipality =="SANTA ISABEL XILOXOXTLA"
replace uniqueid=29027 if municipality =="TENANCINGO"
replace uniqueid=29028 if municipality =="SAN LUIS TEOLOCHOLCO"
replace uniqueid=29019 if municipality =="TEPETITLA DE LARDIZABAL"
replace uniqueid=29029 if municipality =="TEPEYANCO"
replace uniqueid=29030 if municipality =="TERRENATE"
replace uniqueid=29031 if municipality =="TETLA DE LA SOLIDARIDAD"
replace uniqueid=29032 if municipality =="TETLATLAHUCA"
replace uniqueid=29033 if municipality =="TLAXCALA"
replace uniqueid=29034 if municipality =="TLAXCO"
replace uniqueid=29035 if municipality =="TOCATLAN"
replace uniqueid=29036 if municipality =="TOTOLAC"
replace uniqueid=29038 if municipality =="TZOMPANTEPEC"
replace uniqueid=29039 if municipality =="XALOZTOC"
replace uniqueid=29040 if municipality =="XALTOCAN"
replace uniqueid=29042 if municipality =="XICOHTZINCO"
replace uniqueid=29043 if municipality =="YAUHQUEMEHCAN"
replace uniqueid=29044 if municipality =="ZACATELCO"
replace uniqueid=29037 if municipality =="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCDT PJS PRI_PVEM PT_PCDT_PJS PCDT_PJS PT_PCDT PT_PJS)

foreach var in PAN PRI PRD PT PVEM PC PCDT PJS PRI_PVEM PT_PCDT_PJS PCDT_PJS PT_PCDT PT_PJS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCDT inv_mun_PJS inv_mun_PRI_PVEM inv_mun_PT_PCDT_PJS inv_mun_PCDT_PJS inv_mun_PT_PCDT inv_mun_PT_PJS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PCDT_r PJS_r PRI_PVEM_r PT_PCDT_PJS_r PCDT_PJS_r PT_PCDT_r PT_PJS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 

replace winner = "PCDT" if PCDT_r ==1 
replace winner = "PJS" if PJS_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PT_PCDT_PJS" if PT_PCDT_PJS_r ==1 
replace winner = "PCDT_PJS" if PCDT_PJS_r ==1 
replace winner = "PT_PCDT" if PT_PCDT_r ==1 
replace winner = "PT_PJS" if PT_PJS_r ==1 
drop *_r

gen year = 2004
gen month ="November"

sort section

save Tlaxcala_Section_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Ayu_Seccion_2007.xlsx", sheet("Sheet1") firstrow clear

drop if municipality ==""

count if CC1_PRI_PVEM!=.
replace PRI_PVEM = CC1_PRI_PVEM if PRI_PVEM==. & CC1_PRI_PVEM!=.
drop CC1_PRI_PVEM
count if CC2_PRI_PVEM!=.
replace PRI_PVEM = CC2_PRI_PVEM if PRI_PVEM==. & CC2_PRI_PVEM!=.
drop CC2_PRI_PVEM

egen total = rowtotal(PRI PAN_PAC PRI_PVEM PRD PT PVEM PC PCDT PANAL PAS PS PRI_PVEM_PS NOREGISTRADOS EMITIDOS VALIDOS)
drop if total ==0

collapse (sum) PRI - total , by(municipality section)

sort section
merge section using Listanominal2007.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop  NOREGISTRADOS EMITIDOS  

gen   uniqueid= 0
replace uniqueid=29022 if municipality =="ACUAMANALA DE MIGUEL HIDALGO"
replace uniqueid=29001 if municipality =="AMAXAC DE GUERRERO"
replace uniqueid=29002 if municipality =="APETATITLAN DE ANTONIO CARVAJAL"
replace uniqueid=29005 if municipality =="APIZACO"
replace uniqueid=29003 if municipality =="ATLANGATEPEC"
replace uniqueid=29004 if municipality =="ATLTZAYANCA"
replace uniqueid=29045 if municipality =="BENITO JUAREZ"
replace uniqueid=29006 if municipality =="CALPULALPAN"
replace uniqueid=29010 if municipality =="CHIAUTEMPAN"
replace uniqueid=29018 if municipality =="CONTLA DE JUAN CUAMATZI"
replace uniqueid=29008 if municipality =="CUAPIAXTLA"
replace uniqueid=29009 if municipality =="CUAXOMULCO"
replace uniqueid=29007 if municipality =="EL CARMEN TEQUEXQUITLA"
replace uniqueid=29046 if municipality =="EMILIANO ZAPATA"
replace uniqueid=29012 if municipality =="ESPANITA"
replace uniqueid=29013 if municipality =="HUAMANTLA"
replace uniqueid=29014 if municipality =="HUEYOTLIPAN"
replace uniqueid=29015 if municipality =="IXTACUIXTLA DE MARIANO MATAMOROS"
replace uniqueid=29016 if municipality =="IXTENCO"
replace uniqueid=29048 if municipality =="LA MAGDALENA TLALTELULCO"
replace uniqueid=29047 if municipality =="LAZARO CARDENAS"
replace uniqueid=29017 if municipality =="MAZATECOCHCO DE JOSE MARIA MORELOS"
replace uniqueid=29011 if municipality =="MUNOZ DE DOMINGO ARENAS"
replace uniqueid=29021 if municipality =="NANACAMILPA DE MARIANO ARISTA"
replace uniqueid=29023 if municipality =="NATIVITAS"
replace uniqueid=29024 if municipality =="PANOTLA"
replace uniqueid=29041 if municipality =="PAPALOTLA DE XICOHTENCATL"
replace uniqueid=29049 if municipality =="SAN DAMIAN TEXOLOC"
replace uniqueid=29050 if municipality =="SAN FRANCISCO TETLANOHCAN"
replace uniqueid=29051 if municipality =="SAN JERONIMO ZACUALPAN"
replace uniqueid=29052 if municipality =="SAN JOSE TEACALCO"
replace uniqueid=29053 if municipality =="SAN JUAN HUACTZINCO"
replace uniqueid=29054 if municipality =="SAN LORENZO AXOCOMANITLA"
replace uniqueid=29055 if municipality =="SAN LUCAS TECOPILCO"
replace uniqueid=29025 if municipality =="SAN PABLO DEL MONTE"
replace uniqueid=29020 if municipality =="SANCTORUM"
replace uniqueid=29056 if municipality =="SANTA ANA NOPALUCAN"
replace uniqueid=29057 if municipality =="SANTA APOLONIA TEACALCO"
replace uniqueid=29058 if municipality =="SANTA CATARINA AYOMETLA"
replace uniqueid=29059 if municipality =="SANTA CRUZ QUILEHTLA"
replace uniqueid=29026 if municipality =="SANTA CRUZ TLAXCALA"
replace uniqueid=29060 if municipality =="SANTA ISABEL XILOXOXTLA"
replace uniqueid=29027 if municipality =="TENANCINGO"
replace uniqueid=29028 if municipality =="SAN LUIS TEOLOCHOLCO"
replace uniqueid=29019 if municipality =="TEPETITLA DE LARDIZABAL"
replace uniqueid=29029 if municipality =="TEPEYANCO"
replace uniqueid=29030 if municipality =="TERRENATE"
replace uniqueid=29031 if municipality =="TETLA DE LA SOLIDARIDAD"
replace uniqueid=29032 if municipality =="TETLATLAHUCA"
replace uniqueid=29033 if municipality =="TLAXCALA"
replace uniqueid=29034 if municipality =="TLAXCO"
replace uniqueid=29035 if municipality =="TOCATLAN"
replace uniqueid=29036 if municipality =="TOTOLAC"
replace uniqueid=29038 if municipality =="TZOMPANTEPEC"
replace uniqueid=29039 if municipality =="XALOZTOC"
replace uniqueid=29040 if municipality =="XALTOCAN"
replace uniqueid=29042 if municipality =="XICOHTZINCO"
replace uniqueid=29043 if municipality =="YAUHQUEMEHCAN"
replace uniqueid=29044 if municipality =="ZACATELCO"
replace uniqueid=29037 if municipality =="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"

rename VALIDOS valid

foreach var in PRI PAN_PAC PRI_PVEM PRD PT PVEM PC PCDT PANAL PAS PS PRI_PVEM_PS total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PAN_PAC inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCDT inv_mun_PANAL inv_mun_PAS inv_mun_PS inv_mun_PRI_PVEM_PS, gen(PRI_r PAN_PAC_r PRI_PVEM_r PRD_r PT_r PVEM_r PC_r PCDT_r PANAL_r PAS_r PS_r PRI_PVEM_PS_r)
drop inv_mun_*

gen winner = "PAN_PAC" if PAN_PAC_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PCDT" if PCDT_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PS" if PS_r ==1 
replace winner = "PRI_PVEM_PS" if PRI_PVEM_PS_r ==1 
drop *_r

gen year = 2007
gen month ="November"

drop listanominal turnout mun_listanominal mun_turnout

save Tlaxcala_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.

destring panpna - total , replace
drop if total ==. | total==0

**************************************************************************

collapse (sum)  listanominal panpna - total , by (municipality section)

sum pripvem pri pvem if pripvem!=0
* br municipality pripvem pri pvem if pripvem!=0 & pvem !=0
replace pvem = 0 if municipality=="NATIVITAS"
* br municipality pripvem pri pvem if pripvem!=0 & pri !=0
replace pri = 0  if municipality =="TERRENATE"

* Technically "UNIDOS POR IXTACUIXTLA" but only show votes for the PRI
sum pripvemps pri pvem ps if pripvemps!=0
drop pripvemps

sum prips pri ps if prips!=0

sum prdpc prd convergencia if prdpc!=0

sum prdpcpt prd convergencia pt if prdpcpt!=0
replace prdpcpt = prdpcpt + pt  if prdpcpt!=0
replace pt  = 0                 if prdpcpt!=0

sum prdpcpt2 prd convergencia pt if prdpcpt2!=0
* There was simply one mistake
replace prd = 0 if prdpcpt2!=0

sum prdpt  prd pt if prdpt!=0
sum prdpt2 prd pt if prdpt2!=0

sum ptpc pt convergencia if ptpc!=0
* They are all mistakes that do not affect the election outcome
replace pt=0 if ptpc!=0

count if prdpcpt!=0 & prdpcpt2!=0
replace prdpcpt = prdpcpt2 if prdpcpt==0 & prdpcpt2!=0
drop prdpcpt2

count if prdpt!=0 & prdpt2!=0
replace prdpt = prdpt2 if prdpt==0 & prdpt2!=0
drop prdpt2

**************************************************************************

rename panpna         PAN_PANAL
rename pri            PRI
rename pripvem        PRI_PVEM
rename prips          PRI_PST
rename prd            PRD
rename prdpcpt        PRD_PT_PC
rename prdpc          PRD_PC
rename prdpt          PRD_PT
rename ptpc           PT_PC
rename pt             PT
rename convergencia   PC
rename pvem           PVEM
rename pac            PAC
rename pp             PP
rename plt            PLT
rename ppt            PPT
rename ps             PST

gen turnout =  total/listanominal

drop  nulos 

gen   uniqueid= 0
replace uniqueid=29022 if municipality =="ACUAMANALA DE MIGUEL HIDALGO"
replace uniqueid=29001 if municipality =="AMAXAC DE GUERRERO"
replace uniqueid=29002 if municipality =="APETATITLAN DE ANTONIO CARVAJAL"
replace uniqueid=29005 if municipality =="APIZACO"
replace uniqueid=29003 if municipality =="ATLANGATEPEC"
replace uniqueid=29004 if municipality =="ATLTZAYANCA"
replace uniqueid=29045 if municipality =="BENITO JUAREZ"
replace uniqueid=29006 if municipality =="CALPULALPAN"
replace uniqueid=29010 if municipality =="CHIAUTEMPAN"
replace uniqueid=29018 if municipality =="CONTLA DE JUAN CUAMATZI"
replace uniqueid=29008 if municipality =="CUAPIAXTLA"
replace uniqueid=29009 if municipality =="CUAXOMULCO"
replace uniqueid=29007 if municipality =="EL CARMEN TEQUEXQUITLA"
replace uniqueid=29046 if municipality =="EMILIANO ZAPATA"
replace uniqueid=29012 if municipality =="ESPANITA"
replace uniqueid=29013 if municipality =="HUAMANTLA"
replace uniqueid=29014 if municipality =="HUEYOTLIPAN"
replace uniqueid=29015 if municipality =="IXTACUIXTLA DE MARIANO MATAMOROS"
replace uniqueid=29016 if municipality =="IXTENCO"
replace uniqueid=29048 if municipality =="LA MAGDALENA TLALTELULCO"
replace uniqueid=29047 if municipality =="LAZARO CARDENAS"
replace uniqueid=29017 if municipality =="MAZATECOCHCO DE JOSE MARIA MORELOSS"
replace uniqueid=29011 if municipality =="MUNOZ DE DOMINGO ARENAS"
replace uniqueid=29021 if municipality =="NANACAMILPA DE MARIANO ARISTA"
replace uniqueid=29023 if municipality =="NATIVITAS"
replace uniqueid=29024 if municipality =="PANOTLA"
replace uniqueid=29041 if municipality =="PAPALOTLA DE XICOHTENCATL"
replace uniqueid=29049 if municipality =="SAN DAMIAN TEXOLOC"
replace uniqueid=29050 if municipality =="SAN FRANCISCO TETLANOHCAN"
replace uniqueid=29051 if municipality =="SAN JERONIMO ZACUALPAN"
replace uniqueid=29052 if municipality =="SAN JOSE TEACALCO"
replace uniqueid=29053 if municipality =="SAN JUAN HUACTZINCO"
replace uniqueid=29054 if municipality =="SAN LORENZO AXOCOMANITLA"
replace uniqueid=29055 if municipality =="SAN LUCAS TECOPILCO"
replace uniqueid=29025 if municipality =="SAN PABLO DEL MONTE"
replace uniqueid=29020 if municipality =="SANCTORUM DE LAZARO CARDENAS"
replace uniqueid=29056 if municipality =="SANTA ANA NOPALUCAN"
replace uniqueid=29057 if municipality =="SANTA APOLONIA TEACALCO"
replace uniqueid=29058 if municipality =="SANTA CATARINA AYOMETLA"
replace uniqueid=29059 if municipality =="SANTA CRUZ QUILEHTLA"
replace uniqueid=29026 if municipality =="SANTA CRUZ TLAXCALA"
replace uniqueid=29060 if municipality =="SANTA ISABEL XILOXOXTLA"
replace uniqueid=29027 if municipality =="TENANCINGO"
replace uniqueid=29028 if municipality =="TEOLOCHOLCO"
replace uniqueid=29019 if municipality =="TEPETITLA DE LARDIZABAL"
replace uniqueid=29029 if municipality =="TEPEYANCO"
replace uniqueid=29030 if municipality =="TERRENATE"
replace uniqueid=29031 if municipality =="TETLA DE LA SOLIDARIDAD"
replace uniqueid=29032 if municipality =="TETLATLAHUCA"
replace uniqueid=29033 if municipality =="TLAXCALA"
replace uniqueid=29034 if municipality =="TLAXCO"
replace uniqueid=29035 if municipality =="TOCATLAN"
replace uniqueid=29036 if municipality =="TOTOLAC"
replace uniqueid=29038 if municipality =="TZOMPANTEPEC"
replace uniqueid=29039 if municipality =="XALOZTOC"
replace uniqueid=29040 if municipality =="XALTOCAN"
replace uniqueid=29042 if municipality =="XICOHTZINCO"
replace uniqueid=29043 if municipality =="YAUHQUEMEHCAN"
replace uniqueid=29044 if municipality =="ZACATELCO"
replace uniqueid=29037 if municipality =="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"

egen valid = rowtotal(PAN_PANAL PRI PRI_PVEM PRI_PST PRD PRD_PT_PC PRD_PC PRD_PT PT PT_PC PVEM PC PST PAC PP PLT PPT)

foreach var in PAN_PANAL PRI PRI_PVEM PRI_PST PRD PRD_PT_PC PRD_PC PRD_PT PT PT_PC PVEM PC PST PAC PP PLT PPT listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PANAL inv_mun_PRI inv_mun_PRI_PVEM inv_mun_PRI_PST inv_mun_PRD inv_mun_PRD_PT_PC inv_mun_PRD_PC inv_mun_PRD_PT inv_mun_PT inv_mun_PT_PC inv_mun_PVEM inv_mun_PC inv_mun_PST inv_mun_PAC inv_mun_PP inv_mun_PLT inv_mun_PPT, gen(PAN_PANAL_r PRI_r PRI_PVEM_r PRI_PST_r PRD_r PRD_PT_PC_r PRD_PC_r PRD_PT_r PT_r PT_PC_r PVEM_r PC_r PST_r PAC_r PP_r PLT_r PPT_r)
drop inv_mun_*

gen winner = "PAN_PANAL" if PAN_PANAL_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRI_PST" if PRI_PST_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PRD_PC" if PRD_PC_r ==1 
replace winner = "PRD_PT_PC" if PRD_PT_PC_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PT_PC" if PT_PC_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PST" if PST_r ==1 
replace winner = "PAC" if PAC_r ==1 
replace winner = "PP" if PP_r ==1 
replace winner = "PLT" if PLT_r ==1 
replace winner = "PPT" if PPT_r ==1 
drop *_r

gen year = 2010
gen month ="July"

sort section

save Tlaxcala_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Ayu_Seccion_2013.xlsx", sheet("Sheet1") firstrow clear

drop if municipality==""
rename Emitidos total
rename Validos valid

collapse (sum) PAN - valid, by(municipality section Coalition)

gen PAC_PS = PAC + PS if Coalition=="PAC_PS"
replace PAC = 0 if Coalition=="PAC_PS"
replace PS = 0 if Coalition=="PAC_PS"
gen PAN_PAC = PAN + PAC if Coalition=="PAN_PAC"
replace PAN = 0 if Coalition=="PAN_PAC"
replace PAC = 0 if Coalition=="PAN_PAC"
gen PRD_PT = PRD + PT if Coalition=="PRD_PT"
replace PRD = 0 if Coalition=="PRD_PT"
replace PT = 0 if Coalition=="PRD_PT"
gen PRI_PVEM = PRI + PVEM if Coalition=="PRI_PVEM"
replace PRI = 0 if Coalition=="PRI_PVEM"
replace PVEM = 0 if Coalition=="PRI_PVEM"
gen PT_PAC = PT + PAC if Coalition=="PT_PAC"
replace PT = 0 if Coalition=="PT_PAC"
replace PAC = 0 if Coalition=="PT_PAC"
drop Coalition

sort section
merge section using Listanominal2013.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop  NoRegistrados 

replace municipality = upper(municipality)
gen   uniqueid= 0
replace uniqueid=29022 if municipality =="ACUAMANALA DE MIGUEL HIDALGO"
replace uniqueid=29001 if municipality =="AMAXAC DE GUERRERO"
replace uniqueid=29002 if municipality =="APETATITLAN DE ANTONIO CARVAJAL"
replace uniqueid=29005 if municipality =="APIZACO"
replace uniqueid=29003 if municipality =="ATLANGATEPEC"
replace uniqueid=29004 if municipality =="ATLTZAYANCA"
replace uniqueid=29045 if municipality =="BENITO JUAREZ"
replace uniqueid=29006 if municipality =="CALPULALPAN"
replace uniqueid=29010 if municipality =="CHIAUTEMPAN"
replace uniqueid=29018 if municipality =="CONTLA DE JUAN CUAMATZI"
replace uniqueid=29008 if municipality =="CUAPIAXTLA"
replace uniqueid=29009 if municipality =="CUAXOMULCO"
replace uniqueid=29007 if municipality =="EL CARMEN TEQUEXQUITLA"
replace uniqueid=29046 if municipality =="EMILIANO ZAPATA"
replace uniqueid=29012 if municipality =="ESPANITA"
replace uniqueid=29013 if municipality =="HUAMANTLA"
replace uniqueid=29014 if municipality =="HUEYOTLIPAN"
replace uniqueid=29015 if municipality =="IXTACUIXTLA DE MARIANO MATAMOROS"
replace uniqueid=29016 if municipality =="IXTENCO"
replace uniqueid=29048 if municipality =="LA MAGDALENA TLALTELULCO"
replace uniqueid=29047 if municipality =="LAZARO CARDENAS"
replace uniqueid=29017 if municipality =="MAZATECOCHCO DE JOSE MARIA MORELOS"
replace uniqueid=29011 if municipality =="MUNOZ DE DOMINGO ARENAS"
replace uniqueid=29021 if municipality =="NANACAMILPA DE MARIANO ARISTA"
replace uniqueid=29023 if municipality =="NATIVITAS"
replace uniqueid=29024 if municipality =="PANOTLA"
replace uniqueid=29041 if municipality =="PAPALOTLA DE XICOHTENCATL"
replace uniqueid=29049 if municipality =="SAN DAMIAN TEXOLOC"
replace uniqueid=29050 if municipality =="SAN FRANCISCO TETLANOHCAN"
replace uniqueid=29051 if municipality =="SAN JERONIMO ZACUALPAN"
replace uniqueid=29052 if municipality =="SAN JOSE TEACALCO"
replace uniqueid=29053 if municipality =="SAN JUAN HUACTZINCO"
replace uniqueid=29054 if municipality =="SAN LORENZO AXOCOMANITLA"
replace uniqueid=29055 if municipality =="SAN LUCAS TECOPILCO"
replace uniqueid=29025 if municipality =="SAN PABLO DEL MONTE"
replace uniqueid=29020 if municipality =="SANCTORUM DE LAZARO CARDENAS"
replace uniqueid=29056 if municipality =="SANTA ANA NOPALUCAN"
replace uniqueid=29057 if municipality =="SANTA APOLONIA TEACALCO"
replace uniqueid=29058 if municipality =="SANTA CATARINA AYOMETLA"
replace uniqueid=29059 if municipality =="SANTA CRUZ QUILEHTLA"
replace uniqueid=29026 if municipality =="SANTA CRUZ TLAXCALA"
replace uniqueid=29060 if municipality =="SANTA ISABEL XILOXOXTLA"
replace uniqueid=29027 if municipality =="TENANCINGO"
replace uniqueid=29028 if municipality =="TEOLOCHOLCO"
replace uniqueid=29019 if municipality =="TEPETITLA DE LARDIZABAL"
replace uniqueid=29029 if municipality =="TEPEYANCO"
replace uniqueid=29030 if municipality =="TERRENATE"
replace uniqueid=29031 if municipality =="TETLA DE LA SOLIDARIDAD"
replace uniqueid=29032 if municipality =="TETLATLAHUCA"
replace uniqueid=29033 if municipality =="TLAXCALA"
replace uniqueid=29034 if municipality =="TLAXCO"
replace uniqueid=29035 if municipality =="TOCATLAN"
replace uniqueid=29036 if municipality =="TOTOLAC"
replace uniqueid=29038 if municipality =="TZOMPANTEPEC"
replace uniqueid=29039 if municipality =="XALOZTOC"
replace uniqueid=29040 if municipality =="XALTOCAN"
replace uniqueid=29042 if municipality =="XICOHTZINCO"
replace uniqueid=29043 if municipality =="YAUHQUEMEHCAN"
replace uniqueid=29044 if municipality =="ZACATELCO"
replace uniqueid=29037 if municipality =="ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"


foreach var in PAN PRI PRD PT PVEM PC PANAL PAC PS PAC_PS PAN_PAC PRD_PT PRI_PVEM PT_PAC total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAC inv_mun_PS inv_mun_PAC_PS inv_mun_PAN_PAC inv_mun_PRD_PT inv_mun_PRI_PVEM inv_mun_PT_PAC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAC_r PS_r PAC_PS_r PAN_PAC_r PRD_PT_r PRI_PVEM_r PT_PAC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PAC" if PAC_r ==1 
replace winner = "PS" if PS_r ==1 
replace winner = "PAC_PS" if PAC_PS_r ==1 
replace winner = "PAN_PAC" if PAN_PAC_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PT_PAC" if PT_PAC_r ==1 

drop *_r

gen year = 2013
gen month ="July"

sort section

save Tlaxcala_Section_2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Tlaxcala_Section_2001.dta, clear
append using  Tlaxcala_Section_2004.dta
append using  Tlaxcala_Section_2007.dta
append using  Tlaxcala_Section_2010.dta
append using  Tlaxcala_Section_2013.dta

erase Tlaxcala_Section_2001.dta
erase Tlaxcala_Section_2004.dta
erase Tlaxcala_Section_2007.dta
erase Tlaxcala_Section_2010.dta
erase Tlaxcala_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC PFC PAC PJS PST PS Partido_Democrata {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}
replace winner_counter = 2 if winner=="PRI_PST"
replace winner_counter = 1 if winner=="PST"
replace winner = "Partido Democrata" if winner=="Partido_Democrata"

tab winner_counter
count if winner_counter==0

tab winner winner_counter

save Tlaxcala_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Tlaxcala_ALL.dta, replace
