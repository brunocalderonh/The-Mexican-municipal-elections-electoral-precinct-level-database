
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Durango - 1995, 1998, 2001, 2004, 2007, 2010, 2013"


*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1995_No_LN.csv, clear

rename municipio  municipality
rename secion section
drop if municipality=="" & section==.

drop if total==. | total==0

destring pan - total, replace

collapse (sum) pan - total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pfcrn  Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM

* gen turnout =  total/nominal

drop  valida  nulos  noreg

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DUANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

egen valid = rowtotal(PAN PRI PRD Partido_Cardenista PT PVEM)

foreach var in PAN PRI PRD Partido_Cardenista PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_Partido_Cardenista inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_r Partido_Cardenista_r PT_r PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD Partido_Cardenista PT PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =1995
gen month ="July"

save Durango_Section_1995.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1998_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

drop if total==. | total==0

destring pan - total, replace

collapse (sum) pan - total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM

* gen turnout =  total/nominal

drop valida  nulos  noreg 

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURNAGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

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

gen year =1998
gen month ="July"

save Durango_Section_1998.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2001_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

drop if total==. | total==0

destring pan - total, replace

save Ayu_Seccion_2001_No_LN.dta, replace

insheet using Durango_Seccion_2001_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

destring pan - total, replace

drop if total==. | total==0

append using Ayu_Seccion_2001_No_LN.dta

collapse (sum) pan - pt, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  prdpt  PRD_PT
rename  pt     PT
rename  pvem   PVEM
rename  pc     PC
rename  psn    PSN
rename  pas    PAS
rename  pd     PD

* gen turnout =  total/nominal

drop validos  nulos  noreg 

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"


egen valid = rowtotal(PAN PRI PRD_PT PVEM PC PSN PAS PD PRD PT)

foreach var in PAN PRI PRD_PT PVEM PC PSN PAS PD PRD PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PRD_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PD, gen(PAN_r PRI_r PRD_r PT_r PRD_PT_r PVEM_r PC_r PSN_r PAS_r PD_r )
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT PVEM PC PSN PAS PD PRD PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =2001
gen month ="July"

save Durango_Section_2001.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "INE-CI141-2014 Horacio Larreguy Arbesu\pdln10_edms_PEL_2004_2007.xls", sheet("pdln10_edms") firstrow clear

keep if FECHA=="20040704"

rename SEC section
rename LISTA listanominal
collapse (sum) listanominal, by(section)

save ListaNominal2004.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2004_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan - pd total, replace

collapse (sum) pan - pd total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prdpt  PRD_PT
rename  pvem   PVEM
rename  pd     PD
 
merge 1:1 section using ListaNominal2004.dta 
drop if _merge==2
drop _merge
 
gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

egen valid = rowtotal(PAN PRI PRD_PT PVEM PD)

foreach var in PAN PRI PRD_PT PVEM PD listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT inv_mun_PVEM inv_mun_PD, gen(PAN_r PRI_r PRD_PT_r PVEM_r PD_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT PVEM PD {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =2004
gen month ="July"

sort section

save Durango_Section_2004.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "INE-CI141-2014 Horacio Larreguy Arbesu\pdln10_edms_PEL_2004_2007.xls", sheet("pdln10_edms") firstrow clear

keep if FECHA=="20070701"

rename SEC section
rename LISTA listanominal
collapse (sum) listanominal, by(section)

save ListaNominal2007.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

use Ayu_Seccion_2007.dta, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan - pripanalpd, replace

collapse (sum) pan - pas total pripanalpd, by (municipality section)

rename  pan         PAN
rename  pripanal    PRI_PANAL
rename  pripanalpd  PRI_PANAL_PD
rename  prd         PRD
rename  ptpc        PT_PC
rename  pvem        PVEM
rename  pas         PAS
rename  pd          PD

merge 1:1 section using ListaNominal2007.dta 
drop if _merge==2
drop _merge
 
gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMÓN BOLIVAR"
replace uniqueid=10007 if municipality =="GÓMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE  VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVÍ"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PÁNUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PEÑON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SÚCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

egen valid = rowtotal(PAN PRI_PANAL PRI_PANAL_PD PRD PT_PC PVEM PD PAS)

foreach var in PAN PRI_PANAL PRI_PANAL_PD PRD PT_PC PVEM PD PAS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PANAL inv_mun_PRI_PANAL_PD inv_mun_PRD inv_mun_PT_PC inv_mun_PVEM inv_mun_PD inv_mun_PAS, gen(PAN_r PRI_PANAL_r PRI_PANAL_PD_r PRD_r PT_PC_r PVEM_r PD_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PANAL PRI_PANAL_PD PRD PT_PC PVEM PD PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =2007
gen month ="July"

sort section

save Durango_Section_2007.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename municipio  municipality
rename seccion section
rename nominal listanominal
drop if municipality=="" & section==.
drop if total==. | total==0

destring  panprdpc - listanominal, replace

collapse (sum) panprdpc - listanominal, by (municipality section)

rename  panprdpc    PAN_PRD_PC
rename  pripvempanalpd    PRI_PVEM_PANAL_PD
rename  pt    PT

gen turnout =  total/listanominal

drop nulos noregistrados

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMONFORT"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="SIMON BOLIVAR"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTORIA"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORONADO"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUADALUPE"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RIO"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CORDERO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL GALLO"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQUIARO"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRERO"

egen valid = rowtotal(PAN_PRD_PC PRI_PVEM_PANAL_PD PT)

foreach var in PAN_PRD_PC PRI_PVEM_PANAL_PD PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PC inv_mun_PRI_PVEM_PANAL_PD inv_mun_PT, gen(PAN_PRD_PC_r PRI_PVEM_PANAL_PD_r PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD_PC PRI_PVEM_PANAL_PD PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =2010
gen month ="July"

sort section

save Durango_Section_2010.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2013.csv, clear

rename ayuntamiento  municipality
replace municipality = subinstr(municipality, "á", "a", .)
replace municipality = subinstr(municipality, "é", "e", .)
replace municipality = subinstr(municipality, "í", "i", .)
replace municipality = subinstr(municipality, "ó", "o", .)
replace municipality = subinstr(municipality, "ú", "u", .)
replace municipality = subinstr(municipality, "ñ", "n", .)
replace municipality =  upper(municipality)
rename seccion section
drop if municipality=="" & section==.
gen total = valid + noregistrados + nulos
drop if total==. | total==0

destring  listanominal - mc total, replace

collapse (sum) listanominal - mc total, by (municipality section)

rename  pan    PAN
rename  pri_pvem_panal_pd    PRI_PVEM_PANAL_PD
rename  prd    PRD
rename  pt    PT
rename  mc PC

gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=10001 if municipality =="CANATLAN"
replace uniqueid=10002 if municipality =="CANELAS"
replace uniqueid=10003 if municipality =="CONETO DE COMON"
replace uniqueid=10004 if municipality =="CUENCAME"
replace uniqueid=10005 if municipality =="DURANGO"
replace uniqueid=10018 if municipality =="EL ORO"
replace uniqueid=10006 if municipality =="GENERAL SIMON B"
replace uniqueid=10007 if municipality =="GOMEZ PALACIO"
replace uniqueid=10008 if municipality =="GUADALUPE VICTO"
replace uniqueid=10009 if municipality =="GUANACEVI"
replace uniqueid=10010 if municipality =="HIDALGO"
replace uniqueid=10011 if municipality =="INDE"
replace uniqueid=10012 if municipality =="LERDO"
replace uniqueid=10013 if municipality =="MAPIMI"
replace uniqueid=10014 if municipality =="MEZQUITAL"
replace uniqueid=10015 if municipality =="NAZAS"
replace uniqueid=10016 if municipality =="NOMBRE DE DIOS"
replace uniqueid=10039 if municipality =="NUEVO IDEAL"
replace uniqueid=10017 if municipality =="OCAMPO"
replace uniqueid=10019 if municipality =="OTAEZ"
replace uniqueid=10020 if municipality =="PANUCO DE CORON"
replace uniqueid=10021 if municipality =="PENON BLANCO"
replace uniqueid=10022 if municipality =="POANAS"
replace uniqueid=10023 if municipality =="PUEBLO NUEVO"
replace uniqueid=10024 if municipality =="RODEO"
replace uniqueid=10025 if municipality =="SAN BERNARDO"
replace uniqueid=10026 if municipality =="SAN DIMAS"
replace uniqueid=10027 if municipality =="SAN JUAN DE GUA"
replace uniqueid=10028 if municipality =="SAN JUAN DEL RI"
replace uniqueid=10029 if municipality =="SAN LUIS DEL CO"
replace uniqueid=10030 if municipality =="SAN PEDRO DEL G"
replace uniqueid=10031 if municipality =="SANTA CLARA"
replace uniqueid=10032 if municipality =="SANTIAGO PAPASQ"
replace uniqueid=10033 if municipality =="SUCHIL"
replace uniqueid=10034 if municipality =="TAMAZULA"
replace uniqueid=10035 if municipality =="TEPEHUANES"
replace uniqueid=10036 if municipality =="TLAHUALILO"
replace uniqueid=10037 if municipality =="TOPIA"
replace uniqueid=10038 if municipality =="VICENTE GUERRER"

egen valid = rowtotal(PAN PRI_PVEM_PANAL_PD PRD PT PC)

foreach var in PAN PRI_PVEM_PANAL_PD PRD PT PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PANAL_PD inv_mun_PRD inv_mun_PT inv_mun_PC, gen(PAN_r PRI_PVEM_PANAL_PD_r PRD_r PT_r PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL_PD PRD PT PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year =2013
gen month ="July"

sort section

save Durango_Section_2013.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Durango_Section_1995.dta
append using Durango_Section_1998.dta
append using Durango_Section_2001.dta
append using Durango_Section_2004.dta
append using Durango_Section_2007.dta
append using Durango_Section_2010.dta
append using Durango_Section_2013.dta

erase Durango_Section_1995.dta
erase Durango_Section_1998.dta
erase Durango_Section_2001.dta
erase Durango_Section_2004.dta
erase Durango_Section_2007.dta
erase Durango_Section_2010.dta
erase Durango_Section_2013.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0
tab winner winner_counter

save Durango_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Durango_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
