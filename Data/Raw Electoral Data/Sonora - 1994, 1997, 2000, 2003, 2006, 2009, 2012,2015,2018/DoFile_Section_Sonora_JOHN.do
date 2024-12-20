capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Sonora - 1994, 1997, 2000, 2003, 2006, 2009, 2012"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1994_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
egen total =  rowtotal(pan pri pps prd pfcrn parm pdm pt pvem)
drop if total==. | total==0 

destring pan - pvem total, replace

collapse (sum)  pan - pvem total, by (municipality section)
 
rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
* Partido Cardenista
rename pfcrn PartCardenista
rename parm PARM
rename pps  PPS
* Partido Dem�crata Mexicano
rename pdm  PDM
 
gen   uniqueid= 0
replace uniqueid=26001 if municipality =="Aconchi"
replace uniqueid=26002 if municipality =="Agua Prieta"
replace uniqueid=26003 if municipality =="Alamos"
replace uniqueid=26004 if municipality =="Altar"
replace uniqueid=26005 if municipality =="Arivechi"
replace uniqueid=26006 if municipality =="Arizpe"
replace uniqueid=26007 if municipality =="Atil"
replace uniqueid=26008 if municipality =="Bacadehuachi"
replace uniqueid=26009 if municipality =="Bacanora"
replace uniqueid=26010 if municipality =="Bacerac"
replace uniqueid=26011 if municipality =="Bacoachi"
replace uniqueid=26012 if municipality =="Bacum"
replace uniqueid=26013 if municipality =="Banamichi"
replace uniqueid=26014 if municipality =="Baviacora"
replace uniqueid=26015 if municipality =="Bavispe"
replace uniqueid=26071 if municipality =="Benito Ju�rez"
replace uniqueid=26071 if strpos(municipality, "Benito J")>0
replace uniqueid=26016 if municipality =="Benjamin Hill"
replace uniqueid=26017 if municipality =="Caborca"
replace uniqueid=26018 if municipality =="Cajeme"
replace uniqueid=26019 if municipality =="Cananea"
replace uniqueid=26020 if municipality =="Carbo"
replace uniqueid=26022 if municipality =="Cucurpe"
replace uniqueid=26023 if municipality =="Cumpas"
replace uniqueid=26024 if municipality =="Divisaderos"
replace uniqueid=26025 if municipality =="Empalme"
replace uniqueid=26026 if municipality =="Etchojoa"
replace uniqueid=26027 if municipality =="Fronteras"
replace uniqueid=26070 if municipality =="Gral. Plutarco Elias Calles"
replace uniqueid=26028 if municipality =="Granados"
replace uniqueid=26029 if municipality =="Guaymas"
replace uniqueid=26030 if municipality =="Hermosillo"
replace uniqueid=26031 if municipality =="Huachinera"
replace uniqueid=26032 if municipality =="Huasabas"
replace uniqueid=26033 if municipality =="Huatabampo"
replace uniqueid=26034 if municipality =="Huepac"
replace uniqueid=26035 if municipality =="Imuris"
replace uniqueid=26021 if municipality =="La Colorada"
replace uniqueid=26036 if municipality =="Magdalena de Kino"
replace uniqueid=26037 if municipality =="Mazatan"
replace uniqueid=26038 if municipality =="Moctezuma"
replace uniqueid=26039 if municipality =="Naco"
replace uniqueid=26040 if municipality =="Nacori Chico"
replace uniqueid=26041 if municipality =="Nacozari de Garcia"
replace uniqueid=26042 if municipality =="Navojoa"
replace uniqueid=26043 if municipality =="Nogales"
replace uniqueid=26044 if municipality =="Onavas"
replace uniqueid=26045 if municipality =="Opodepe"
replace uniqueid=26046 if municipality =="Oquitoa"
replace uniqueid=26047 if municipality =="Pitiquito"
replace uniqueid=26048 if municipality =="Puerto Pe�asco"
replace uniqueid=26048 if strpos(municipality, "Puerto P")>0
replace uniqueid=26049 if municipality =="Quiriego"
replace uniqueid=26050 if municipality =="Rayon"
replace uniqueid=26051 if municipality =="Rosario de Tesopaco"
replace uniqueid=26052 if municipality =="Sahuaripa"
replace uniqueid=26053 if municipality =="San Felipe de Jesus"
replace uniqueid=26054 if municipality =="San Javier"
replace uniqueid=26055 if municipality =="San Luis Rio Colorado"
replace uniqueid=26056 if municipality =="San Miguel De Horcasitas"
replace uniqueid=26057 if municipality =="San Pedro De La Cueva"
replace uniqueid=26058 if municipality =="Santa Ana"
replace uniqueid=26059 if municipality =="Santa Cruz"
replace uniqueid=26060 if municipality =="Saric"
replace uniqueid=26061 if municipality =="Soyopa"
replace uniqueid=26062 if municipality =="Suaqui Grande"
replace uniqueid=26063 if municipality =="Tepache"
replace uniqueid=26064 if municipality =="Trincheras"
replace uniqueid=26065 if municipality =="Tubutama"
replace uniqueid=26066 if municipality =="Ures"
replace uniqueid=26067 if municipality =="Villa Hidalgo"
replace uniqueid=26068 if municipality =="Villa Pesqueira"
replace uniqueid=26069 if municipality =="Yecora"

egen valid = rowtotal(PAN PRI PPS PRD PartCardenista PARM PDM PT PVEM)

foreach var in PAN PRI PPS PRD PartCardenista PARM PDM PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 26
g sec = section
capture merge 1:m ed sec using "..\Nationwide Listanominal 1994.dta", keepusing(lista)
drop if _merge==2
drop _merge
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PPS inv_mun_PRD inv_mun_PartCardenista inv_mun_PARM inv_mun_PDM inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PPS_r PRD_r PartCardenista_r PARM_r PDM_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PARM" if PARM_r ==1 
replace winner = "PDM" if PDM_r ==1 
replace winner = "PartCardenista" if PartCardenista_r ==1 
replace winner = "PPS" if PPS_r ==1 

drop *_r

gen year = 1994
gen month ="August"

save Sonora_Section_1994.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1997_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal
drop if municipality=="" & section==.
egen total =  rowtotal(pan pri prd pc pt pvem pps pdm)
drop if total==. | total==0 

destring pan - pdm total, replace

collapse (sum)  pan - pdm total, by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
* Partido Cardenista
rename pc   PartCardenista
rename pps  PPS
* Partido Dem�crata Mexicano
rename pdm  PDM

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BACUM"
replace uniqueid=26013 if municipality =="BANAMICHI"
replace uniqueid=26014 if municipality =="BAVIACORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUAREZ"
replace uniqueid=26016 if municipality =="BENJAMIN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBO"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="PLUTARCO ELIAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUASABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUEPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATAN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NACORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCIA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PEÑASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYON"
replace uniqueid=26051 if municipality =="ROSARIO TESOPACO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESUS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RIO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RIO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SARIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YECORA"


egen valid = rowtotal(PAN PRI PRD PartCardenista PT PVEM PPS PDM)

foreach var in PAN PRI PRD PartCardenista PT PVEM PPS PDM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 26
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==7 & year==1997
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartCardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PDM, gen(PAN_r PRI_r PRD_r PartCardenista_r PT_r PVEM_r PPS_r PDM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PartCardenista" if PartCardenista_r ==1 
replace winner = "PPS" if PPS_r ==1 
replace winner = "PDM" if PDM_r ==1 
drop *_r

gen year = 1997
gen month ="July"

save Sonora_Section_1997.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel Ayu_Seccion_2000.xlsx, clear firstrow case(lower)

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
destring  pan - listanominal , replace
egen total =  rowtotal(pan pri prd pt pvem pc pcd psn parm pas pds  nulos)
drop if total==. | total==0 

collapse (sum)   pan - listanominal total, by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
* Partido de Centro Democr�tico
rename pcd  PCD
rename psn  PSN
rename parm  PARM
rename pas  PAS
rename pds  PDS

gen turnout =  total/listanominal

drop  validos nulos  

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ÁLAMOS"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ÁTIL"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BÁCUM"
replace uniqueid=26013 if municipality =="BANÁMICHI"
replace uniqueid=26014 if municipality =="BAVIÁCORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUÁREZ"
replace uniqueid=26016 if municipality =="BENJAMÍN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBÓ"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL . PLUTARCO ELÍAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUÁSABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUÉPAC"
replace uniqueid=26035 if municipality =="ÍMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA DE KINO"
replace uniqueid=26037 if municipality =="MAZATÁN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NÁCORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCÍA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ÓNAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PEÑASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYÓN"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESÚS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RÍO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RÍO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SÁRIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YÉCORA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)

foreach var in PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCD inv_mun_PSN inv_mun_PARM inv_mun_PAS inv_mun_PDS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PCD_r PSN_r PARM_r PAS_r PDS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PCD" if PCD_r ==1 
replace winner = "PSN" if PSN_r ==1 
replace winner = "PARM" if PARM_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PDS" if PDS_r ==1 
drop *_r

gen year = 2000
gen month ="July"

save Sonora_Section_2000.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2003.csv, clear

rename municipio  municipality
rename casilla section

drop if municipality=="" & section==.

destring  pan - listanominal , replace

**************************************************************************

gen dummy_PAN_PRD = 1  if  pan_prd!=. 
gen dummy_PARD 	= 1 if pard!=.
gen dummy_PRI_PVEM	 	= 1  if pri_pvem!=. 
gen dummy_PRD_PT_PC	= 1  if  prd_pt_pc!=. 
gen dummy_PAS_PMP	= 1  if pas_pmp!=. 
gen dummy_PRD_PC_PAS	= 1  if prd_pc_pas!=. 
gen dummy_PSN_PAS	= 1  if psn_pas!=. 
gen dummy_PRD_PT= 1  if prd_pt!=.

sum pan_prd pan prd if pan_prd!=.
replace pan_prd = pan + prd + pan_prd if dummy_PAN_PRD==1
replace pan = 0 if dummy_PAN_PRD==1
replace prd = 0 if dummy_PAN_PRD==1

sum pard pan prd if pard!=.
replace pan_prd = pan + prd + pard if dummy_PARD==1
replace pan = 0 if dummy_PARD==1
replace prd = 0 if dummy_PARD==1
drop pard 

sum pri_pvem pri pvem if pri_pvem!=.
replace pri_pvem = pri + pvem + pri_pvem if dummy_PRI_PVEM==1
replace pri = 0  if dummy_PRI_PVEM==1
replace pvem = 0 if dummy_PRI_PVEM==1

sum prd_pt_pc prd pt pc  if prd_pt_pc!=.
replace prd_pt_pc = prd + pt + pc + prd_pt_pc if dummy_PRD_PT_PC == 1 
replace prd = 0 if dummy_PRD_PT_PC == 1 
replace pt = 0 if dummy_PRD_PT_PC == 1 
replace pc = 0 if dummy_PRD_PT_PC == 1 

sum pas pmp pas_pmp if pas_pmp!=.
replace pas_pmp = pas + pmp + pas_pmp if dummy_PAS_PMP == 1
replace pas = 0 if dummy_PAS_PMP == 1
replace pmp = 0 if dummy_PAS_PMP == 1

sum prd_pc_pas prd pc pas if prd_pc_pas!=.
replace prd_pc_pas = prd + pc + pas + prd_pc_pas if dummy_PRD_PC_PAS == 1
replace prd = 0 if dummy_PRD_PC_PAS == 1
replace pc = 0 if dummy_PRD_PC_PAS == 1
replace pas = 0 if dummy_PRD_PC_PAS == 1

sum psn_pas psn pas if psn_pas!=.
replace psn_pas= psn + pas + psn_pas if dummy_PSN_PAS == 1
replace psn = 0 if dummy_PSN_PAS == 1
replace pas = 0 if dummy_PSN_PAS == 1

sum prd_pt prd pt  if prd_pt!=.
replace prd_pt = prd_pt + prd + pt if dummy_PRD_PT == 1
replace prd  =0 if dummy_PRD_PT == 1
replace pt =0 if dummy_PRD_PT == 1

**************************************************************************

collapse (sum)   pan - nulos (first) listanominal , by (municipality section)

egen total =  rowtotal(pan pri prd pvem pt pas pfc pc pmp psn pan_prd pri_pvem prd_pt_pc pas_pmp prd_pc_pas psn_pas prd_pt nulos)
 
drop if total==0 

rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pvem PVEM
rename pas PAS
rename pfc PFC
rename pc PC
rename pmp PMP
rename psn PSN
rename pan_prd  PAN_PRD
rename pri_pvem  PRI_PVEM
rename prd_pt_pc  PRD_PT_PC
rename pas_pmp  PAS_PMP
rename prd_pc_pas  PRD_PC_PAS
rename psn_pas  PSN_PAS
rename prd_pt PRD_PT

gen turnout =  total/listanominal

drop  validos nulos  

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BACUM"
replace uniqueid=26013 if municipality =="BANAMICHI"
replace uniqueid=26014 if municipality =="BAVIACORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUAREZ"
replace uniqueid=26016 if municipality =="BENJAMIN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBO"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL. PLUTARCO ELIAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUASABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUEPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA DE KINO"
replace uniqueid=26037 if municipality =="MAZATAN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NACORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCIA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PE�ASCO"
replace uniqueid=26048 if strpos(municipality, "PUERTO P")>0
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYON"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESUS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RIO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RIO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SARIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YECORA"

egen valid = rowtotal(PAN PRI PRD PVEM PT PAS PFC PC PMP PSN PAN_PRD PRI_PVEM PRD_PT_PC PAS_PMP PRD_PC_PAS PSN_PAS PRD_PT)

foreach var in PAN PRI PRD PVEM PT PAS PFC PC PMP PSN PAN_PRD PRI_PVEM PRD_PT_PC PAS_PMP PRD_PC_PAS PSN_PAS PRD_PT listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PT inv_mun_PAS inv_mun_PFC inv_mun_PC inv_mun_PMP inv_mun_PSN inv_mun_PAN_PRD inv_mun_PRI_PVEM inv_mun_PRD_PT_PC inv_mun_PAS_PMP inv_mun_PRD_PC_PAS inv_mun_PSN_PAS inv_mun_PRD_PT, gen(PAN_r PRI_r PRD_r PVEM_r PT_r PAS_r PFC_r PC_r PMP_r PSN_r PAN_PRD_r PRI_PVEM_r PRD_PT_PC_r PAS_PMP_r PRD_PC_PAS_r PSN_PAS_r PRD_PT_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PFC" if PFC_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PMP" if PMP_r ==1 
replace winner = "PSN" if PSN_r ==1 
replace winner = "PAN_PRD" if PAN_PRD_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD_PT_PC" if PRD_PT_PC_r ==1 
replace winner = "PAS_PMP" if PAS_PMP_r ==1 
replace winner = "PRD_PC_PAS" if PRD_PC_PAS_r ==1 
replace winner = "PSN_PAS" if PSN_PAS_r ==1
replace winner = "PRD_PT" if PRD_PT_r ==1
drop *_r

gen year = 2003
gen month ="July"

sort section

save Sonora_Section_2003.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel Ayu_Seccion_2006.xlsx, clear firstrow case(lower)

rename municipio  municipality
rename casilla section
rename totalvotos  total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - listanominal , replace

collapse (sum)   pan - listanominal , by (municipality section)

rename pan PAN
rename pri PRI
rename pripanal PRI_PANAL
rename prdpt PRD_PT
rename pvem PVEM
rename convergencia PC
rename alternativa PAS

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ÁLAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ÁTIL"
replace uniqueid=26008 if municipality =="BACADÉHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BÁCUM"
replace uniqueid=26013 if municipality =="BANÁMICHI"
replace uniqueid=26014 if municipality =="BAVIÁCORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUÁREZ"
replace uniqueid=26016 if municipality =="BENJAMÍN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBÓ"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GENERAL PLUTARCO ELÍAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUÁSABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUÉPAC"
replace uniqueid=26035 if municipality =="ÍMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATÁN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NÁCORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCÍA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ÓNAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PEÑASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYÓN"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESÚS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RÍO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RÍO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SÁRIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="ÚRES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YÉCORA"


egen valid = rowtotal(PAN PRI PRI_PANAL PRD_PT PVEM PC PAS)

foreach var in PAN PRI PRI_PANAL PRD_PT PVEM PC PAS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRI_PANAL inv_mun_PRD_PT inv_mun_PVEM inv_mun_PC inv_mun_PAS, gen(PAN_r PRI_r PRI_PANAL_r PRD_PT_r PVEM_r PC_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PAS" if PAS_r ==1 
drop *_r

gen year = 2006
gen month ="July"

sort section

save Sonora_Section_2006.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan - total , replace

collapse (sum) listanominal  pan - total , by (municipality section)

rename pan PAN
rename pri PRI
rename pripanalpvem PRI_PVEM_PANAL
rename prd PRD
rename pt PT
rename pvem PVEM
rename pc PC
rename psd PSD

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BACUM"
replace uniqueid=26013 if municipality =="BANAMICHI"
replace uniqueid=26014 if municipality =="BAVIACORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUAREZ"
replace uniqueid=26016 if municipality =="BENJAMIN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBO"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL. PLUTARCO ELIAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUASABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUEPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATAN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NACORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCIA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PE?ASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYON"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESUS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RIO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RIO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SARIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YECORA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSD PRI_PVEM_PANAL)

foreach var in PAN PRI PRD PT PVEM PC PSD PRI_PVEM_PANAL listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSD inv_mun_PRI_PVEM_PANAL, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSD_r PRI_PVEM_PANAL_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PSD" if PSD_r ==1 
drop *_r

gen year = 2009
gen month ="July"

sort section

save Sonora_Section_2009.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2012.xlsx", sheet("ELECCION AYUNTAMIENTO") firstrow clear

drop DISTRITO CASILLA 
rename MUNICIPIO municipality
rename SECCION section
rename LISTANOMINAL listanominal

drop if municipality==""

foreach var in PAN_PANAL PRI_PVEM PRD_PT PRD_PC PRD_PT_PC  {
bys municipality: egen COAL_`var' = sum(`var')
replace COAL_`var' = (COAL_`var'>0)
}

* Things are added up in the excel
replace PAN = . if COAL_PAN_PANAL==1
replace PANAL = . if COAL_PAN_PANAL==1
replace PRI = . if COAL_PRI_PVEM==1
replace PVEM = . if COAL_PRI_PVEM==1
replace PRD = . if COAL_PRD_PT==1 | COAL_PRD_PC ==1 | COAL_PRD_PT_PC ==1 
replace PT = . if COAL_PRD_PT==1 | COAL_PRD_PT_PC ==1 
replace PC = . if COAL_PRD_PC ==1 | COAL_PRD_PT_PC ==1 

drop CC_PAN_PANAL CC_PRI_PVEM CC_PRD_PT CC_PRD_PC CC_PRD_PT_PC
drop COAL_PAN_PANAL COAL_PRI_PVEM COAL_PRD_PT COAL_PRD_PC COAL_PRD_PT_PC

replace PRI_PVEM = PRIPVEM if PRI_PVEM==. & PRIPVEM!=.
drop PRIPVEM

egen total = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PANAL PRI_PVEM PRD_PT PRD_PC PRD_PT_PC VOTOSNULOS)

collapse (sum) PAN - total , by(municipality section)

drop if total==0

gen turnout =  total/listanominal

drop  VOTOSNULOS  

gen   uniqueid= 0
replace uniqueid=26001 if municipality =="ACONCHI"
replace uniqueid=26002 if municipality =="AGUA PRIETA"
replace uniqueid=26003 if municipality =="ALAMOS"
replace uniqueid=26004 if municipality =="ALTAR"
replace uniqueid=26005 if municipality =="ARIVECHI"
replace uniqueid=26006 if municipality =="ARIZPE"
replace uniqueid=26007 if municipality =="ATIL"
replace uniqueid=26008 if municipality =="BACADEHUACHI"
replace uniqueid=26009 if municipality =="BACANORA"
replace uniqueid=26010 if municipality =="BACERAC"
replace uniqueid=26011 if municipality =="BACOACHI"
replace uniqueid=26012 if municipality =="BACUM"
replace uniqueid=26013 if municipality =="BANAMICHI"
replace uniqueid=26014 if municipality =="BAVIACORA"
replace uniqueid=26015 if municipality =="BAVISPE"
replace uniqueid=26071 if municipality =="BENITO JUAREZ"
replace uniqueid=26016 if municipality =="BENJAMIN HILL"
replace uniqueid=26017 if municipality =="CABORCA"
replace uniqueid=26018 if municipality =="CAJEME"
replace uniqueid=26019 if municipality =="CANANEA"
replace uniqueid=26020 if municipality =="CARBO"
replace uniqueid=26022 if municipality =="CUCURPE"
replace uniqueid=26023 if municipality =="CUMPAS"
replace uniqueid=26024 if municipality =="DIVISADEROS"
replace uniqueid=26025 if municipality =="EMPALME"
replace uniqueid=26026 if municipality =="ETCHOJOA"
replace uniqueid=26027 if municipality =="FRONTERAS"
replace uniqueid=26070 if municipality =="GRAL. PLUTARCO ELIAS CALLES"
replace uniqueid=26028 if municipality =="GRANADOS"
replace uniqueid=26029 if municipality =="GUAYMAS"
replace uniqueid=26030 if municipality =="HERMOSILLO"
replace uniqueid=26031 if municipality =="HUACHINERA"
replace uniqueid=26032 if municipality =="HUASABAS"
replace uniqueid=26033 if municipality =="HUATABAMPO"
replace uniqueid=26034 if municipality =="HUEPAC"
replace uniqueid=26035 if municipality =="IMURIS"
replace uniqueid=26021 if municipality =="LA COLORADA"
replace uniqueid=26036 if municipality =="MAGDALENA"
replace uniqueid=26037 if municipality =="MAZATAN"
replace uniqueid=26038 if municipality =="MOCTEZUMA"
replace uniqueid=26039 if municipality =="NACO"
replace uniqueid=26040 if municipality =="NACORI CHICO"
replace uniqueid=26041 if municipality =="NACOZARI DE GARCIA"
replace uniqueid=26042 if municipality =="NAVOJOA"
replace uniqueid=26043 if municipality =="NOGALES"
replace uniqueid=26044 if municipality =="ONAVAS"
replace uniqueid=26045 if municipality =="OPODEPE"
replace uniqueid=26046 if municipality =="OQUITOA"
replace uniqueid=26047 if municipality =="PITIQUITO"
replace uniqueid=26048 if municipality =="PUERTO PENASCO"
replace uniqueid=26049 if municipality =="QUIRIEGO"
replace uniqueid=26050 if municipality =="RAYON"
replace uniqueid=26051 if municipality =="ROSARIO"
replace uniqueid=26052 if municipality =="SAHUARIPA"
replace uniqueid=26053 if municipality =="SAN FELIPE DE JESUS"
replace uniqueid=26072 if municipality =="SAN IGNACIO RIO MUERTO"
replace uniqueid=26054 if municipality =="SAN JAVIER"
replace uniqueid=26055 if municipality =="SAN LUIS RIO COLORADO"
replace uniqueid=26056 if municipality =="SAN MIGUEL DE HORCASITAS"
replace uniqueid=26057 if municipality =="SAN PEDRO DE LA CUEVA"
replace uniqueid=26058 if municipality =="SANTA ANA"
replace uniqueid=26059 if municipality =="SANTA CRUZ"
replace uniqueid=26060 if municipality =="SARIC"
replace uniqueid=26061 if municipality =="SOYOPA"
replace uniqueid=26062 if municipality =="SUAQUI GRANDE"
replace uniqueid=26063 if municipality =="TEPACHE"
replace uniqueid=26064 if municipality =="TRINCHERAS"
replace uniqueid=26065 if municipality =="TUBUTAMA"
replace uniqueid=26066 if municipality =="URES"
replace uniqueid=26067 if municipality =="VILLA HIDALGO"
replace uniqueid=26068 if municipality =="VILLA PESQUEIRA"
replace uniqueid=26069 if municipality =="YECORA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PAN_PANAL PRI_PVEM PRD_PT PRD_PC PRD_PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PANAL PAN_PANAL PRI_PVEM PRD_PT PRD_PC PRD_PT_PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PAN_PANAL inv_mun_PRI_PVEM inv_mun_PRD_PT inv_mun_PRD_PC inv_mun_PRD_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PAN_PANAL_r PRI_PVEM_r PRD_PT_r PRD_PC_r PRD_PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PAN_PANAL" if PAN_PANAL_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PRD_PC" if PRD_PC_r ==1 
replace winner = "PRD_PT_PC" if PRD_PT_PC_r ==1 
drop *_r

gen year = 2012
gen month ="July"

sort section

save Sonora_Section_2012.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Sonora_Section_1994.dta
append using Sonora_Section_1997.dta
append using Sonora_Section_2000.dta
append using Sonora_Section_2003.dta
append using Sonora_Section_2006.dta
append using Sonora_Section_2009.dta
append using Sonora_Section_2012.dta

erase Sonora_Section_1994.dta
erase Sonora_Section_1997.dta
erase Sonora_Section_2000.dta
erase Sonora_Section_2003.dta
erase Sonora_Section_2006.dta
erase Sonora_Section_2009.dta
erase Sonora_Section_2012.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC PFC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

saveold Sonora_ALL.dta, replace version(12)

saveold "..\Sonora_ALL.dta", replace version(12)
