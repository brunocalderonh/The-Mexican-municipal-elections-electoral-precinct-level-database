

clear all
set mem 1g

capture cd "C:\Users\jmarshall\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Yucatan 1995, 1998, 2001, 2004, 2007, 2010, 2012"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1995.csv, clear

rename municipio  municipality
rename secc section
rename emitidos total

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - listanominal , replace

collapse (sum)  pan - listanominal , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pfcrn   PartCardenista 
rename otros Otros

gen turnout =  total/listanominal

drop  nulos   

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="Abalá"
replace uniqueid=31002 if municipality =="Acanceh"
replace uniqueid=31003 if municipality =="Akil"
replace uniqueid=31004 if municipality =="Baca"
replace uniqueid=31005 if municipality =="Bokobá"
replace uniqueid=31006 if municipality =="Buctzotz"
replace uniqueid=31007 if municipality =="Cacalchen"
replace uniqueid=31008 if municipality =="Calotmul"
replace uniqueid=31009 if municipality =="Cansahcab"
replace uniqueid=31010 if municipality =="Cantamayec"
replace uniqueid=31011 if municipality =="Celestun"
replace uniqueid=31012 if municipality =="Cenotillo"
replace uniqueid=31016 if municipality =="Chacsinkin"
replace uniqueid=31017 if municipality =="Chankom"
replace uniqueid=31018 if municipality =="Chapab"
replace uniqueid=31019 if municipality =="Chemax"
replace uniqueid=31021 if municipality =="Chichimila"
replace uniqueid=31020 if municipality =="Chicxulub Pueblo"
replace uniqueid=31022 if municipality =="Chikidzonot"
replace uniqueid=31023 if municipality =="Chocholá"
replace uniqueid=31024 if municipality =="Chumayel"
replace uniqueid=31013 if municipality =="Conkal"
replace uniqueid=31014 if municipality =="Cuncunul"
replace uniqueid=31015 if municipality =="Cuzama"
replace uniqueid=31025 if municipality =="Dzan"
replace uniqueid=31026 if municipality =="Dzemul"
replace uniqueid=31027 if municipality =="Dzidzantun"
replace uniqueid=31028 if municipality =="Dzilam Bravo"
replace uniqueid=31029 if municipality =="Dzilam Gonzalez"
replace uniqueid=31030 if municipality =="Dzitas"
replace uniqueid=31031 if municipality =="Dzoncauich"
replace uniqueid=31032 if municipality =="Espita"
replace uniqueid=31033 if municipality =="Halachó"
replace uniqueid=31034 if municipality =="Hocaba"
replace uniqueid=31035 if municipality =="Hoctun"
replace uniqueid=31036 if municipality =="Homun"
replace uniqueid=31037 if municipality =="Huhi"
replace uniqueid=31038 if municipality =="Hunucma"
replace uniqueid=31039 if municipality =="Ixil"
replace uniqueid=31040 if municipality =="Izamal"
replace uniqueid=31041 if municipality =="Kanasin"
replace uniqueid=31042 if municipality =="Kantunil"
replace uniqueid=31043 if municipality =="Kaua"
replace uniqueid=31044 if municipality =="Kinchil"
replace uniqueid=31045 if municipality =="Kopomá"
replace uniqueid=31046 if municipality =="Mama"
replace uniqueid=31047 if municipality =="Mani"
replace uniqueid=31048 if municipality =="Maxcanú"
replace uniqueid=31049 if municipality =="Mayapan"
replace uniqueid=31050 if municipality =="Mérida"
replace uniqueid=31051 if municipality =="Mococha"
replace uniqueid=31052 if municipality =="Motul"
replace uniqueid=31053 if municipality =="Muna"
replace uniqueid=31054 if municipality =="Muxupip"
replace uniqueid=31055 if municipality =="Opichen"
replace uniqueid=31056 if municipality =="Oxkutzcab"
replace uniqueid=31057 if municipality =="Panaba"
replace uniqueid=31058 if municipality =="Peto"
replace uniqueid=31059 if municipality =="Progreso"
replace uniqueid=31060 if municipality =="Quintana Roo"
replace uniqueid=31061 if municipality =="Rio lagartos"
replace uniqueid=31062 if municipality =="Sacalum"
replace uniqueid=31063 if municipality =="Samahil"
replace uniqueid=31065 if municipality =="San Felipe"
replace uniqueid=31064 if municipality =="Sanahcat"
replace uniqueid=31066 if municipality =="Santa Elena"
replace uniqueid=31067 if municipality =="Seye"
replace uniqueid=31068 if municipality =="Sinanche"
replace uniqueid=31069 if municipality =="Sotuta"
replace uniqueid=31070 if municipality =="Sucila"
replace uniqueid=31071 if municipality =="Sudzal"
replace uniqueid=31072 if municipality =="Suma de Hidalgo"
replace uniqueid=31073 if municipality =="Tahdziu"
replace uniqueid=31074 if municipality =="Tahmek"
replace uniqueid=31075 if municipality =="Teabo"
replace uniqueid=31076 if municipality =="Tecoh"
replace uniqueid=31077 if municipality =="Tekal de Venegas"
replace uniqueid=31078 if municipality =="Tekanto"
replace uniqueid=31079 if municipality =="Tekax"
replace uniqueid=31080 if municipality =="Tekit"
replace uniqueid=31081 if municipality =="Tekom"
replace uniqueid=31082 if municipality =="Telchac Pueblo"
replace uniqueid=31083 if municipality =="Telchac Puerto"
replace uniqueid=31084 if municipality =="Temax"
replace uniqueid=31085 if municipality =="Temozón"
replace uniqueid=31086 if municipality =="Tepakan"
replace uniqueid=31087 if municipality =="Tetiz"
replace uniqueid=31088 if municipality =="Teya"
replace uniqueid=31089 if municipality =="Ticul"
replace uniqueid=31090 if municipality =="Timucuy"
replace uniqueid=31091 if municipality =="Tinum"
replace uniqueid=31092 if municipality =="Tixcacalcupul"
replace uniqueid=31093 if municipality =="Tixkokob"
replace uniqueid=31094 if municipality =="Tixmehuac"
replace uniqueid=31095 if municipality =="Tixpehual"
replace uniqueid=31096 if municipality =="Tizimin"
replace uniqueid=31097 if municipality =="Tunkas"
replace uniqueid=31098 if municipality =="Tzucacab"
replace uniqueid=31099 if municipality =="Uayma"
replace uniqueid=31100 if municipality =="Ucu"
replace uniqueid=31101 if municipality =="Umán"
replace uniqueid=31102 if municipality =="Valladolid"
replace uniqueid=31103 if municipality =="Xocchel"
replace uniqueid=31104 if municipality =="Yaxcaba"
replace uniqueid=31105 if municipality =="Yaxkukul"
replace uniqueid=31106 if municipality =="Yobain"

egen valid = rowtotal(PAN PRI PRD PartCardenista PT PVEM Otros)

foreach var in PAN PRI PRD PartCardenista PT PVEM Otros listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartCardenista inv_mun_PT inv_mun_PVEM inv_mun_Otros, gen(PAN_r PRI_r PRD_r PartCardenista_r PT_r PVEM_r Otros_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PartCardenista" if PartCardenista_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "Otros" if Otros_r ==1 
drop *_r

gen year = 1995
gen month ="May"

save Yucatan_Section_1995.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1998_No_LN.csv, clear

rename municipio  municipality
rename secc section

drop if municipality=="" & section==.
egen total =  rowtotal(pan  pri  prd  pt  pvem  noregistrados nulos)
replace total=. if total==0 

destring  pan - nulos total, replace
collapse (sum)  pan - nulos total, by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM

drop  nulos  noreg  

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALA"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBA"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHEN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTUN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKIN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILA"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLA"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMA"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTUN"
replace uniqueid=31028 if municipality =="DZILAM DE BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHO"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOMUN"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCMA"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASIN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMA"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANI"
replace uniqueid=31048 if municipality =="MAXCANU"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHA"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABA"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILA"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA DE HIDALGO"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZON"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEHUAC"
replace uniqueid=31095 if municipality =="TIXPEHUAL"
replace uniqueid=31096 if municipality =="TIZIMIN"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCU"
replace uniqueid=31101 if municipality =="UMAN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAIN"

egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 31
g seccion = section
capture merge 1:m ed seccion using "C:\Users\Horacio\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
capture merge 1:m ed seccion using "C:\Users\jmarshall\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
keep if month==6 & year==1998
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM , gen(PAN_r PRI_r PRD_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
drop *_r

gen year = 1998
gen month ="May"

save Yucatan_Section_1998.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001.csv, clear

rename municipio  municipality
rename secc section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal - total , replace

collapse (sum)  listanominal - total , by (municipality section)

**************************************************************************

gen pan_pvem = pan + pvem if municipality=="TEKAX"
replace pan = 0 if municipality=="TEKAX"
replace pvem = 0 if municipality=="TEKAX"

gen prd_pt_pvem = prd + pt + pvem if municipality=="UMAN" | municipality=="VALLADOLID"
replace prd = 0 if municipality=="UMAN" | municipality=="VALLADOLID"
replace pt = 0 if municipality=="UMAN" | municipality=="VALLADOLID"
replace pvem  = 0 if municipality=="UMAN" | municipality=="VALLADOLID"

gen pt_cpd = pt + cpd if municipality=="MOTUL"
replace pt  = 0 if municipality=="MOTUL"
replace cpd  = 0 if municipality=="MOTUL"

gen pt_pvem = pt + pvem  if municipality=="HUNUCMA"
replace pt  = 0   if municipality=="HUNUCMA"
replace pvem = 0  if municipality=="HUNUCMA"

gen pan_prd = pan + prd if municipality=="BACA" | municipality=="BOKOBA" | municipality=="BUCTZOTZ" | municipality=="CALOTMUL" | municipality=="CELESTUN" | municipality=="CENOTILLO" | municipality=="CHOCHOLA" | municipality=="CHUMAYEL" | municipality=="DZAN" | municipality=="DZEMUL" | municipality=="DZILAM BRAVO" | municipality=="DZILAM GONZALEZ" | municipality=="DZONCAUICH" | municipality=="ESPITA" | municipality=="HOCABA" | municipality=="KANASIN" | municipality=="KOPOMA" | municipality=="MAMA" | municipality=="MANI" | municipality=="MAYAPAN" | municipality=="MOTUL" | municipality=="MUXUPIP" | municipality=="MERIDA" | municipality=="OPICHEN" | municipality=="PETO" | municipality=="PROGRESO" | municipality=="QUINTANA ROO" | municipality=="RIO LAGARTOS" | municipality=="SAN FELIPE" | municipality=="SANAHCAT" | municipality=="SANTA ELENA" | municipality=="SUDZAL" | municipality=="TAHDZIU" | municipality=="TEKAL DE VENEGAS" | municipality=="TEKANTO" | municipality=="TELCHAC PUERTO" | municipality=="TEPAKAN" | municipality=="TIMUCUY" | municipality=="TIXKOKOB" | municipality=="TIXMEUAC" | municipality=="TIZIMIN" | municipality=="UAYMA" | municipality=="YAXCABA" | municipality=="YAXKUKUL" | municipality=="YOBAIN"  
replace pan = 0 if  municipality=="BACA" | municipality=="BOKOBA" | municipality=="BUCTZOTZ" | municipality=="CALOTMUL" | municipality=="CELESTUN" | municipality=="CENOTILLO" | municipality=="CHOCHOLA" | municipality=="CHUMAYEL" | municipality=="DZAN" | municipality=="DZEMUL" | municipality=="DZILAM BRAVO" | municipality=="DZILAM GONZALEZ" | municipality=="DZONCAUICH" | municipality=="ESPITA" | municipality=="HOCABA" | municipality=="KANASIN" | municipality=="KOPOMA" | municipality=="MAMA" | municipality=="MANI" | municipality=="MAYAPAN" | municipality=="MOTUL" | municipality=="MUXUPIP" | municipality=="MERIDA" | municipality=="OPICHEN" | municipality=="PETO" | municipality=="PROGRESO" | municipality=="QUINTANA ROO" | municipality=="RIO LAGARTOS" | municipality=="SAN FELIPE" | municipality=="SANAHCAT" | municipality=="SANTA ELENA" | municipality=="SUDZAL" | municipality=="TAHDZIU" | municipality=="TEKAL DE VENEGAS" | municipality=="TEKANTO" | municipality=="TELCHAC PUERTO" | municipality=="TEPAKAN" | municipality=="TIMUCUY" | municipality=="TIXKOKOB" | municipality=="TIXMEUAC" | municipality=="TIZIMIN" | municipality=="UAYMA" | municipality=="YAXCABA" | municipality=="YAXKUKUL" | municipality=="YOBAIN" 
replace prd = 0 if municipality=="BACA" | municipality=="BOKOBA" | municipality=="BUCTZOTZ" | municipality=="CALOTMUL" | municipality=="CELESTUN" | municipality=="CENOTILLO" | municipality=="CHOCHOLA" | municipality=="CHUMAYEL" | municipality=="DZAN" | municipality=="DZEMUL" | municipality=="DZILAM BRAVO" | municipality=="DZILAM GONZALEZ" | municipality=="DZONCAUICH" | municipality=="ESPITA" | municipality=="HOCABA" | municipality=="KANASIN" | municipality=="KOPOMA" | municipality=="MAMA" | municipality=="MANI" | municipality=="MAYAPAN" | municipality=="MOTUL" | municipality=="MUXUPIP" | municipality=="MERIDA" | municipality=="OPICHEN" | municipality=="PETO" | municipality=="PROGRESO" | municipality=="QUINTANA ROO" | municipality=="RIO LAGARTOS" | municipality=="SAN FELIPE" | municipality=="SANAHCAT" | municipality=="SANTA ELENA" | municipality=="SUDZAL" | municipality=="TAHDZIU" | municipality=="TEKAL DE VENEGAS" | municipality=="TEKANTO" | municipality=="TELCHAC PUERTO" | municipality=="TEPAKAN" | municipality=="TIMUCUY" | municipality=="TIXKOKOB" | municipality=="TIXMEUAC" | municipality=="TIZIMIN" | municipality=="UAYMA" | municipality=="YAXCABA" | municipality=="YAXKUKUL" | municipality=="YOBAIN" 

**************************************************************************

rename pan  PAN
rename pan_prd PAN_PRD
rename pan_pvem PAN_PVEM
rename pri  PRI
rename prd  PRD
rename prd_pt_pvem  PRD_PT_PVEM
rename pt   PT
rename pt_cpd   PT_PC
rename pt_pvem  PT_PVEM
rename pvem PVEM
rename cpd PC
rename pas PAS
rename py PY
rename pay PAY
rename otros Otros

gen turnout =  total/listanominal

drop  nulos   

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALA"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBA"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHEN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTUN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKIN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILA"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLA"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMA"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTUN"
replace uniqueid=31028 if municipality =="DZILAM BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHO"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOMUN"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCMA"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASIN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMA"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANI"
replace uniqueid=31048 if municipality =="MAXCANU"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHA"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABÁ"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILÁ"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA DE HIDALGO"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZON"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEUAC"
replace uniqueid=31095 if municipality =="TIXPEHUAL"
replace uniqueid=31096 if municipality =="TIZIMIN"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCU"
replace uniqueid=31101 if municipality =="UMAN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAIN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PAS PY PAY Otros PAN_PVEM PRD_PT_PVEM PT_PC PT_PVEM PAN_PRD)

foreach var in PAN PRI PRD PT PVEM PC PAS PY PAY Otros PAN_PVEM PRD_PT_PVEM PT_PC PT_PVEM PAN_PRD listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PAS inv_mun_PY inv_mun_PAY inv_mun_Otros inv_mun_PAN_PVEM inv_mun_PRD_PT_PVEM inv_mun_PT_PC inv_mun_PT_PVEM inv_mun_PAN_PRD, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PAS_r PY_r PAY_r Otros_r PAN_PVEM_r PRD_PT_PVEM_r PT_PC_r PT_PVEM_r PAN_PRD_r)
drop inv_mun_*

gen winner = ""
foreach var in PAN PRI PRD PT PVEM PC PAS PY PAY Otros PAN_PVEM PRD_PT_PVEM PT_PC PT_PVEM PAN_PRD {
replace winner = "`var'" if `var'_r ==1 
}
drop *_r

gen year = 2001
gen month ="May"

save Yucatan_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004.csv, clear

rename municipio  municipality
rename secc section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal - total , replace

collapse (sum)  listanominal - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename pripvem  PRI_PVEM
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename conv PC
rename py PY
rename pay PAY

replace PRI_PVEM = PRI + PVEM + PRI_PVEM if municipality=="Tekit"
replace PRI = 0 if municipality=="Tekit" 
replace PVEM = 0 if municipality=="Tekit" 

gen turnout =  total/listanominal

drop  nulos  noreg  

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="Abalá"
replace uniqueid=31002 if municipality =="Acanceh"
replace uniqueid=31003 if municipality =="Akil"
replace uniqueid=31004 if municipality =="Baca"
replace uniqueid=31005 if municipality =="Bokobá"
replace uniqueid=31006 if municipality =="Buctzotz"
replace uniqueid=31007 if municipality =="Cacalchén"
replace uniqueid=31008 if municipality =="Calotmul"
replace uniqueid=31009 if municipality =="Cansahcab"
replace uniqueid=31010 if municipality =="Cantamayec"
replace uniqueid=31011 if municipality =="Celestún"
replace uniqueid=31012 if municipality =="Cenotillo"
replace uniqueid=31016 if municipality =="Chacsinkín"
replace uniqueid=31017 if municipality =="Chankom"
replace uniqueid=31018 if municipality =="Chapab"
replace uniqueid=31019 if municipality =="Chemax"
replace uniqueid=31021 if municipality =="Chichimilá"
replace uniqueid=31020 if municipality =="Chicxulub Pueblo"
replace uniqueid=31022 if municipality =="Chikindzonot"
replace uniqueid=31023 if municipality =="Chocholá"
replace uniqueid=31024 if municipality =="Chumayel"
replace uniqueid=31013 if municipality =="Conkal"
replace uniqueid=31014 if municipality =="Cuncunul"
replace uniqueid=31015 if municipality =="Cuzumá"
replace uniqueid=31025 if municipality =="Dzan"
replace uniqueid=31026 if municipality =="Dzemul"
replace uniqueid=31027 if municipality =="Dzidzantún"
replace uniqueid=31028 if municipality =="Dizlam de Bravo"
replace uniqueid=31029 if municipality =="Dzilam González"
replace uniqueid=31030 if municipality =="Dzitás"
replace uniqueid=31031 if municipality =="Dzoncauich"
replace uniqueid=31032 if municipality =="Espita"
replace uniqueid=31033 if municipality =="Halachó"
replace uniqueid=31034 if municipality =="Hocabá"
replace uniqueid=31035 if municipality =="Hoctún"
replace uniqueid=31036 if municipality =="Homún"
replace uniqueid=31037 if municipality =="Huhí"
replace uniqueid=31038 if municipality =="Hunucmá"
replace uniqueid=31039 if municipality =="Ixil"
replace uniqueid=31040 if municipality =="Izamal"
replace uniqueid=31041 if municipality =="Kanasín"
replace uniqueid=31042 if municipality =="Kantunil"
replace uniqueid=31043 if municipality =="Kaua"
replace uniqueid=31044 if municipality =="Kinchil"
replace uniqueid=31045 if municipality =="Kopomá"
replace uniqueid=31046 if municipality =="Mama"
replace uniqueid=31047 if municipality =="Maní"
replace uniqueid=31048 if municipality =="Maxcanú"
replace uniqueid=31049 if municipality =="Mayapán"
replace uniqueid=31050 if municipality =="Mérida"
replace uniqueid=31051 if municipality =="Mocochá"
replace uniqueid=31052 if municipality =="Motul"
replace uniqueid=31053 if municipality =="Muna"
replace uniqueid=31054 if municipality =="Muxupip"
replace uniqueid=31055 if municipality =="Opichén"
replace uniqueid=31056 if municipality =="Oxkutzcab"
replace uniqueid=31057 if municipality =="Panabá"
replace uniqueid=31058 if municipality =="Peto"
replace uniqueid=31059 if municipality =="Progreso"
replace uniqueid=31060 if municipality =="Quintana Roo"
replace uniqueid=31061 if municipality =="Río Lagartos"
replace uniqueid=31062 if municipality =="Sacalum"
replace uniqueid=31063 if municipality =="Samahil"
replace uniqueid=31065 if municipality =="San Felipe"
replace uniqueid=31064 if municipality =="Sanahcat"
replace uniqueid=31066 if municipality =="Santa Elena"
replace uniqueid=31067 if municipality =="Seyé"
replace uniqueid=31068 if municipality =="Sinanché"
replace uniqueid=31069 if municipality =="Sotuta"
replace uniqueid=31070 if municipality =="Sucilá"
replace uniqueid=31071 if municipality =="Sudzal"
replace uniqueid=31072 if municipality =="Suma de Hidalgo"
replace uniqueid=31073 if municipality =="Tahdziú"
replace uniqueid=31074 if municipality =="Tahmek"
replace uniqueid=31075 if municipality =="Teabo"
replace uniqueid=31076 if municipality =="Tecoh"
replace uniqueid=31077 if municipality =="Tekal de Venegas"
replace uniqueid=31078 if municipality =="Tekantó"
replace uniqueid=31079 if municipality =="Tekax"
replace uniqueid=31080 if municipality =="Tekit"
replace uniqueid=31081 if municipality =="Tekom"
replace uniqueid=31082 if municipality =="Telchac Pueblo"
replace uniqueid=31083 if municipality =="Telchac Puerto"
replace uniqueid=31084 if municipality =="Temax"
replace uniqueid=31085 if municipality =="Temozón"
replace uniqueid=31086 if municipality =="Tepakán"
replace uniqueid=31087 if municipality =="Tetiz"
replace uniqueid=31088 if municipality =="Teya"
replace uniqueid=31089 if municipality =="Ticul"
replace uniqueid=31090 if municipality =="Timucuy"
replace uniqueid=31091 if municipality =="Tinum"
replace uniqueid=31092 if municipality =="Tixcacalcupul"
replace uniqueid=31093 if municipality =="Tixkokob"
replace uniqueid=31094 if municipality =="Tixmehuac"
replace uniqueid=31095 if municipality =="Tixpehual"
replace uniqueid=31096 if municipality =="Tizimín"
replace uniqueid=31097 if municipality =="Tunkás"
replace uniqueid=31098 if municipality =="Tzucacab"
replace uniqueid=31099 if municipality =="Uayma"
replace uniqueid=31100 if municipality =="Ucú"
replace uniqueid=31101 if municipality =="Umán"
replace uniqueid=31102 if municipality =="Valladolid"
replace uniqueid=31103 if municipality =="Xocchel"
replace uniqueid=31104 if municipality =="Yaxcabá"
replace uniqueid=31105 if municipality =="Yaxkukul"
replace uniqueid=31106 if municipality =="Yobaín"

egen valid = rowtotal(PAN PRI PRI_PVEM PRD PT PVEM PC PY PAY)

foreach var in PAN PRI PRI_PVEM PRD PT PVEM PC PY PAY  listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PY inv_mun_PAY, gen(PAN_r PRI_r PRI_PVEM_r PRD_r PT_r PVEM_r PC_r PY_r PAY_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PY" if PY_r ==1 
replace winner = "PAY" if PAY_r ==1 
drop *_r

gen year = 2004
gen month ="May"

sort section

save Yucatan_Section_2004.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)

replace  nulosdirectos =  nulosdirectos + marginal1 + marginal2 + marginal3 + marginal4 
drop marginal*
rename nulosdirectos nulos

gen ptpc = pt + pc
drop pt pc

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename ptpc   PT_PC
rename pvem PVEM
rename pay PAY
rename panal PANAL
rename pas PAS
rename ci CI

gen turnout =  total/listanominal

drop  nulos  

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALA"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBA"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHEN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTUN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKIN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILA"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLA"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMA"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTUN"
replace uniqueid=31028 if municipality =="DZILAM DE BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHO"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOMUN"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCMA"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASIN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMA"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANI"
replace uniqueid=31048 if municipality =="MAXCANU"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHA"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABA"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILA"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZON"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEHUAC"
replace uniqueid=31095 if municipality =="TIXPEHUAL"
replace uniqueid=31096 if municipality =="TIZIMIN"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCU"
replace uniqueid=31101 if municipality =="UMAN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAIN"

egen valid = rowtotal(PAN PRI PRD PVEM PAY PANAL PAS CI PT_PC)

foreach var in PAN PRI PRD PVEM PAY PANAL PAS CI PT_PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PAY inv_mun_PANAL inv_mun_PAS inv_mun_CI inv_mun_PT_PC, gen(PAN_r PRI_r PRD_r PVEM_r PAY_r PANAL_r PAS_r CI_r PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT_PC" if PT_PC_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PAY" if PAY_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "CI" if CI_r ==1 
drop *_r

gen year = 2007
gen month ="May"

sort section

save Yucatan_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)

replace pripvem = pri + pvem +  pripvem if  municipality== "CELEST¿N" | | municipality== "DZIDZANT¿N" | municipality== "ESPITA" | municipality== "IZAMAL" | municipality== "KANAS¿N" | municipality== "MERIDA" | municipality== "PETO" | municipality== "SEYE" | municipality== "TEKAX" | municipality== "TEMOZ¿N" | municipality== "TICUL" | municipality== "TINUM" | municipality== "TIZIM¿N" | municipality== "UM¿N" | municipality== "VALLADOLID" | municipality== "YAXCABA" 
replace pri = 0 if municipality== "CELEST¿N" | | municipality== "DZIDZANT¿N" | municipality== "ESPITA" | municipality== "IZAMAL" | municipality== "KANAS¿N" | municipality== "MERIDA" | municipality== "PETO" | municipality== "SEYE" | municipality== "TEKAX" | municipality== "TEMOZ¿N" | municipality== "TICUL" | municipality== "TINUM" | municipality== "TIZIM¿N" | municipality== "UM¿N" | municipality== "VALLADOLID" | municipality== "YAXCABA" 
replace pvem = 0 if municipality== "CELEST¿N" | | municipality== "DZIDZANT¿N" | municipality== "ESPITA" | municipality== "IZAMAL" | municipality== "KANAS¿N" | municipality== "MERIDA" | municipality== "PETO" | municipality== "SEYE" | municipality== "TEKAX" | municipality== "TEMOZ¿N" | municipality== "TICUL" | municipality== "TINUM" | municipality== "TIZIM¿N" | municipality== "UM¿N" | municipality== "VALLADOLID" | municipality== "YAXCABA" 

rename pan  PAN
rename pri  PRI
rename pri  PRI_PVEM
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pay PAY
rename panal PANAL
rename pc PC

gen turnout =  total/listanominal

drop  nulos  noregistrados 

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABAL¿"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOB¿"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCH¿N"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELEST¿N"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINK¿N"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMIL¿"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOL¿"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAM¿"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANT¿N"
replace uniqueid=31028 if municipality =="DZILAM DE BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACH¿"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOM¿N"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCM¿"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANAS¿N"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOM¿"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MAN¿"
replace uniqueid=31048 if municipality =="MAXCAN¿"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCH¿"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICH¿N"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANAB¿"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCH¿"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCIL¿"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZ¿N"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPUL"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEHUAC"
replace uniqueid=31095 if municipality =="TIXPEHUAL"
replace uniqueid=31096 if municipality =="TIZIM¿N"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UC¿"
replace uniqueid=31101 if municipality =="UM¿N"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBA¿N"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PAY PANAL PRI_PVEM)

foreach var in PAN PRI PRD PT PVEM PC PAY PANAL PRI_PVEM listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PAY inv_mun_PANAL inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PAY_r PANAL_r PRI_PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
drop *_r

* special election held in Mama due to a tied election, which the PRI went on to win (although there are not seccion-level results available)
replace winner = "PRI" if municipality=="MAMA"

gen year = 2010
gen month ="May"

sort section

save Yucatan_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


import excel "Coaltion_2012.xlsx", sheet("Sheet1") firstrow clear

foreach party in PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC {
replace `party' = 0 if  `party'==.
replace `party' = (`party'>0)
rename  `party' c_`party'
}

rename MUNICIPIO municipality

replace municipality="TIXCACALCUPU" if strpos(municipality,"TIXCACALCUPU")>0

save Coaltion_2012.dta, replace

******************************************************************************************
******************************************************************************************
******************************************************************************************


import excel "Ayu_Seccion_2012.xlsx", sheet("Sheet1") firstrow clear

drop if Municipio==""
count if Listanominal==.

rename Municipio municipality
rename Seccin section
rename Listanominal  listanominal
rename TOTAL  total

drop if municipality=="" & section==.
drop if total==. | total==0 

replace municipality="TIXCACALCUPU" if strpos(municipality,"TIXCACALCUPU")>0
merge m:1 municipality using Coaltion_2012.dta
drop _merge

*foreach party in PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC {
*bys municipality: egen  c_`party'= sum(`party')  
*replace c_`party' = (c_`party'>0) 
*}

*gen c_sum =  c_PRI_PVEM + c_PRI_PVEM_PSD + c_PRD_MC + c_PT_PRD_MC + c_PT_PRD_PSD + c_PT_PRD + c_PT_MC
* br c_* if c_sum==2
* br c_* if c_sum==3
* drop c_sum

* I have some inconistencies due to the possbility of voting for many coalitions in the ballot for the same candidate

gen t_PRI_PVEM  = PRI_PVEM + PRI + PVEM if c_PRI_PVEM ==1
replace PRI_PVEM = 0 if c_PRI_PVEM ==1
replace PRI = 0  if c_PRI_PVEM ==1
replace PVEM = 0 if c_PRI_PVEM ==1

gen t_PRI_PVEM_PSD = PRI_PVEM + PRI + PVEM + PSD if c_PRI_PVEM_PSD ==1
replace PRI_PVEM_PSD = 0 if c_PRI_PVEM_PSD ==1
replace PRI = 0  if c_PRI_PVEM_PSD ==1
replace PVEM = 0 if c_PRI_PVEM_PSD ==1
replace PSDYUC = 0  if c_PRI_PVEM_PSD ==1

gen t_PRD_MC  = PRD_MC + PRD + MC if c_PRD_MC ==1
replace PRD_MC = 0 if c_PRD_MC ==1
replace PRD = 0  if c_PRD_MC ==1
replace MC = 0 if c_PRD_MC ==1 
 
gen t_PT_PRD_MC  = PT_PRD_MC + PT + PRD + MC if c_PT_PRD_MC ==1
replace PT_PRD_MC = 0 if c_PT_PRD_MC ==1
replace PT = 0  if c_PT_PRD_MC ==1
replace PRD = 0  if c_PT_PRD_MC ==1
replace MC = 0 if c_PT_PRD_MC ==1 

gen t_PT_PRD_PSD  = PT_PRD_PSD + PT + PRD + PSD if c_PT_PRD_PSD ==1
replace PT_PRD_PSD = 0 if c_PT_PRD_PSD ==1
replace PT = 0  if c_PT_PRD_PSD ==1
replace PRD = 0  if c_PT_PRD_PSD ==1
replace PSDYUC = 0 if c_PT_PRD_PSD ==1 
 
gen t_PT_PRD  = PT_PRD + PT + PRD if c_PT_PRD ==1
replace PT_PRD = 0 if c_PT_PRD ==1
replace PT = 0 if c_PT_PRD ==1 
replace PRD = 0  if c_PT_PRD ==1

gen t_PT_MC  = PT_MC + PT + MC if c_PT_MC ==1
replace PT_MC = 0 if c_PT_MC ==1
replace PT = 0 if c_PT_MC ==1 
replace MC = 0  if c_PT_MC ==1
 
drop PRI_PVEM_PSD PT_PRD_MC PT_PRD_PSD  PT_PRD

* I checked the cases and all clearly seem as mistakes
* (or the parties vote shares are so so small that it does not matter)
* tab muni if PRD_MC>0
drop PRD_MC

* I checked the cases and all clearly seem as mistakes
* (or the parties vote shares are so so small that it does not matter)
* tab muni if PT_MC>0
drop PT_MC

* tab muni if PRI_PVEM>0
* I checked the cases and all clearly seem as mistakes
* (or the parties vote shares are so so small that it does not matter)
* The only exception is XOCCHEL but the winner confirms this is OK.
drop PRI_PVEM

drop c_*

foreach party in PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC {
rename t_`party' `party'
}


destring  listanominal PAN - PT_MC , replace

collapse (sum)  listanominal PAN - PT_MC , by (municipality section)

gen turnout =  total/listanominal

drop  NOREGISTRADOS NULOS  

gen   uniqueid= 0
replace uniqueid=31001 if municipality =="ABALA"
replace uniqueid=31002 if municipality =="ACANCEH"
replace uniqueid=31003 if municipality =="AKIL"
replace uniqueid=31004 if municipality =="BACA"
replace uniqueid=31005 if municipality =="BOKOBA"
replace uniqueid=31006 if municipality =="BUCTZOTZ"
replace uniqueid=31007 if municipality =="CACALCHEN"
replace uniqueid=31008 if municipality =="CALOTMUL"
replace uniqueid=31009 if municipality =="CANSAHCAB"
replace uniqueid=31010 if municipality =="CANTAMAYEC"
replace uniqueid=31011 if municipality =="CELESTUN"
replace uniqueid=31012 if municipality =="CENOTILLO"
replace uniqueid=31016 if municipality =="CHACSINKIN"
replace uniqueid=31017 if municipality =="CHANKOM"
replace uniqueid=31018 if municipality =="CHAPAB"
replace uniqueid=31019 if municipality =="CHEMAX"
replace uniqueid=31021 if municipality =="CHICHIMILA"
replace uniqueid=31020 if municipality =="CHICXULUB PUEBLO"
replace uniqueid=31022 if municipality =="CHIKINDZONOT"
replace uniqueid=31023 if municipality =="CHOCHOLA"
replace uniqueid=31024 if municipality =="CHUMAYEL"
replace uniqueid=31013 if municipality =="CONKAL"
replace uniqueid=31014 if municipality =="CUNCUNUL"
replace uniqueid=31015 if municipality =="CUZAMA"
replace uniqueid=31025 if municipality =="DZAN"
replace uniqueid=31026 if municipality =="DZEMUL"
replace uniqueid=31027 if municipality =="DZIDZANTUN"
replace uniqueid=31028 if municipality =="DZILAM DE BRAVO"
replace uniqueid=31029 if municipality =="DZILAM GONZALEZ"
replace uniqueid=31030 if municipality =="DZITAS"
replace uniqueid=31031 if municipality =="DZONCAUICH"
replace uniqueid=31032 if municipality =="ESPITA"
replace uniqueid=31033 if municipality =="HALACHO"
replace uniqueid=31034 if municipality =="HOCABA"
replace uniqueid=31035 if municipality =="HOCTUN"
replace uniqueid=31036 if municipality =="HOMUN"
replace uniqueid=31037 if municipality =="HUHI"
replace uniqueid=31038 if municipality =="HUNUCMA"
replace uniqueid=31039 if municipality =="IXIL"
replace uniqueid=31040 if municipality =="IZAMAL"
replace uniqueid=31041 if municipality =="KANASIN"
replace uniqueid=31042 if municipality =="KANTUNIL"
replace uniqueid=31043 if municipality =="KAUA"
replace uniqueid=31044 if municipality =="KINCHIL"
replace uniqueid=31045 if municipality =="KOPOMA"
replace uniqueid=31046 if municipality =="MAMA"
replace uniqueid=31047 if municipality =="MANI"
replace uniqueid=31048 if municipality =="MAXCANU"
replace uniqueid=31049 if municipality =="MAYAPAN"
replace uniqueid=31050 if municipality =="MERIDA"
replace uniqueid=31051 if municipality =="MOCOCHA"
replace uniqueid=31052 if municipality =="MOTUL"
replace uniqueid=31053 if municipality =="MUNA"
replace uniqueid=31054 if municipality =="MUXUPIP"
replace uniqueid=31055 if municipality =="OPICHEN"
replace uniqueid=31056 if municipality =="OXKUTZCAB"
replace uniqueid=31057 if municipality =="PANABA"
replace uniqueid=31058 if municipality =="PETO"
replace uniqueid=31059 if municipality =="PROGRESO"
replace uniqueid=31060 if municipality =="QUINTANA ROO"
replace uniqueid=31061 if municipality =="RIO LAGARTOS"
replace uniqueid=31062 if municipality =="SACALUM"
replace uniqueid=31063 if municipality =="SAMAHIL"
replace uniqueid=31065 if municipality =="SAN FELIPE"
replace uniqueid=31064 if municipality =="SANAHCAT"
replace uniqueid=31066 if municipality =="SANTA ELENA"
replace uniqueid=31067 if municipality =="SEYE"
replace uniqueid=31068 if municipality =="SINANCHE"
replace uniqueid=31069 if municipality =="SOTUTA"
replace uniqueid=31070 if municipality =="SUCILA"
replace uniqueid=31071 if municipality =="SUDZAL"
replace uniqueid=31072 if municipality =="SUMA"
replace uniqueid=31073 if municipality =="TAHDZIU"
replace uniqueid=31074 if municipality =="TAHMEK"
replace uniqueid=31075 if municipality =="TEABO"
replace uniqueid=31076 if municipality =="TECOH"
replace uniqueid=31077 if municipality =="TEKAL DE VENEGAS"
replace uniqueid=31078 if municipality =="TEKANTO"
replace uniqueid=31079 if municipality =="TEKAX"
replace uniqueid=31080 if municipality =="TEKIT"
replace uniqueid=31081 if municipality =="TEKOM"
replace uniqueid=31082 if municipality =="TELCHAC PUEBLO"
replace uniqueid=31083 if municipality =="TELCHAC PUERTO"
replace uniqueid=31084 if municipality =="TEMAX"
replace uniqueid=31085 if municipality =="TEMOZON"
replace uniqueid=31086 if municipality =="TEPAKAN"
replace uniqueid=31087 if municipality =="TETIZ"
replace uniqueid=31088 if municipality =="TEYA"
replace uniqueid=31089 if municipality =="TICUL"
replace uniqueid=31090 if municipality =="TIMUCUY"
replace uniqueid=31091 if municipality =="TINUM"
replace uniqueid=31092 if municipality =="TIXCACALCUPU"
replace uniqueid=31093 if municipality =="TIXKOKOB"
replace uniqueid=31094 if municipality =="TIXMEHUAC"
replace uniqueid=31095 if municipality =="TIXPEHUAL"
replace uniqueid=31096 if municipality =="TIZIMIN"
replace uniqueid=31097 if municipality =="TUNKAS"
replace uniqueid=31098 if municipality =="TZUCACAB"
replace uniqueid=31099 if municipality =="UAYMA"
replace uniqueid=31100 if municipality =="UCU"
replace uniqueid=31101 if municipality =="UMAN"
replace uniqueid=31102 if municipality =="VALLADOLID"
replace uniqueid=31103 if municipality =="XOCCHEL"
replace uniqueid=31104 if municipality =="YAXCABA"
replace uniqueid=31105 if municipality =="YAXKUKUL"
replace uniqueid=31106 if municipality =="YOBAIN"

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL PSDYUC PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC)

foreach var in PAN PRI PRD PT PVEM MC PANAL PSDYUC PRI_PVEM PRI_PVEM_PSD PRD_MC PT_PRD_MC PT_PRD_PSD PT_PRD PT_MC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_MC inv_mun_PANAL inv_mun_PSDYUC inv_mun_PRI_PVEM inv_mun_PRI_PVEM_PSD inv_mun_PRD_MC inv_mun_PT_PRD_MC inv_mun_PT_PRD_PSD inv_mun_PT_PRD inv_mun_PT_MC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r PSDYUC_r PRI_PVEM_r PRI_PVEM_PSD_r PRD_MC_r PT_PRD_MC_r PT_PRD_PSD_r PT_PRD_r PT_MC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "MC" if MC_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PSDYUC" if PSDYUC_r ==1 
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRI_PVEM_PSD" if PRI_PVEM_PSD_r ==1 
replace winner = "PRD_MC" if PRD_MC_r ==1 
replace winner = "PT_PRD_MC" if PT_PRD_MC_r ==1 
replace winner = "PT_PRD_PSD" if PT_PRD_PSD_r ==1
replace winner = "PT_PRD" if PT_PRD_r ==1
replace winner = "PT_MC" if PT_MC_r ==1

drop *_r

gen year = 2012
gen month ="July"

save Yucatan_Section_2012.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Yucatan_Section_1995.dta
append using  Yucatan_Section_1998.dta
append using  Yucatan_Section_2001.dta
append using  Yucatan_Section_2004.dta
append using  Yucatan_Section_2007.dta
append using  Yucatan_Section_2010.dta
append using  Yucatan_Section_2012.dta

erase Yucatan_Section_1995.dta
erase Yucatan_Section_1998.dta
erase Yucatan_Section_2001.dta
erase Yucatan_Section_2004.dta
erase Yucatan_Section_2007.dta
erase Yucatan_Section_2010.dta
erase Yucatan_Section_2012.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartCardenista  CDPPN PUP PEC PFC PAC PJS PST PRV PY PAY CI MC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

saveold Yucatan_ALL.dta, replace version(12)

capture cd "C:\Users\jmarshall\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct"

saveold Yucatan_ALL.dta, replace version(12)

