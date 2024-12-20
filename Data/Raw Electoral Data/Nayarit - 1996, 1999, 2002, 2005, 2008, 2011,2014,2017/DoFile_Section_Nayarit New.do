
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Nayarit - 1996, 1999, 2002, 2005, 2008, 2011 - Missing 2014 (but PREP available; requested it)"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nayarit - 1996, 1999, 2002, 2005, 2008, 2011 - Missing 2014 (but PREP available; requested it)"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1996_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
egen total = rowtotal(pan pri prd pt pvem prs parm cd noregistrados nulos)
*drop if total==. | total==0 

destring pan -  nulos, replace

collapse (sum) pan -  nulos total, by (municipality section)
  
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM
rename  prs    PRS
rename  parm   PARM
* Partido de Centro Democrático
rename  cd     PCD

* gen turnout =  total/listanominal

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLAN"
replace uniqueid=18003 if municipality =="AMATLAN DE CAÑAS"
replace uniqueid=18020 if municipality =="BAHIA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="EL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLAN DEL RIO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUIZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"


egen valid = rowtotal(PAN PRI PRD PT PVEM PRS PARM PCD)

foreach var in PAN PRI PRD PT PVEM PRS PARM PCD total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PRS inv_mun_PARM inv_mun_PCD, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PRS_r PARM_r PCD_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PRS PARM PCD {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1996
gen month ="July"

save Nayarit_Section_1996.dta, replace

keep  municipality section uniqueid
sort section 

save Nayarit_Section_1996_to_Merge_with_1999.dta, replace

use Nayarit_Section_1996.dta, clear

drop if total==. | total==0 

save Nayarit_Section_1996.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


insheet using Ayu_Seccion_1999_No_LN.csv, clear

rename nomunicipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality==. & section==.
egen total = rowtotal(pri pvem medp parmen pps cac candnoreg votosnulos)
drop if total==. | total==0 

destring  pri - votosnulos, replace

collapse (sum)  pri - votosnulos total, by (municipality section)

rename  cac    PAN_PRD_PT
rename  pri    PRI
rename  pvem   PVEM
* Partido del Movimiento Electoral del Pueblo
rename  medp   PMEP
rename  parm   PARM
rename  pps    PPS

* gen turnout =  total/listanominal

drop   candnoreg votosnulos 

egen valid = rowtotal(PRI PVEM PMEP PARM PPS PAN_PRD_PT)

foreach var in PRI PVEM PMEP PARM PPS PAN_PRD_PT total valid{
bys municipality: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PVEM inv_mun_PMEP inv_mun_PARM inv_mun_PPS inv_mun_PAN_PRD_PT, gen(PRI_r PVEM_r PMEP_r PARM_r PPS_r PAN_PRD_PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PVEM PMEP PARM PPS PAN_PRD_PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1999
gen month ="July"

drop municipality
sort section
merge section using Nayarit_Section_1996_to_Merge_with_1999.dta
drop if _merge==2
drop _merge

sort section

save Nayarit_Section_1999.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2002_No_LN.csv, clear

rename nombre_municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  total, replace

collapse (sum) pan -  pas total, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pt     PT
rename  pvem   PVEM
rename  prs    PRS
rename  medp   PMEP
rename  cdppn  PC
rename  psn    PSN
rename  pas    PAS

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLAN"
replace uniqueid=18003 if municipality =="AMATLAN DE CA?AS"
replace uniqueid=18020 if municipality =="BAHIA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="EL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLAN DEL RIO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUIZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"

egen valid = rowtotal(PAN PRI PRD PT PVEM PRS PMEP PC PSN PAS)

foreach var in PAN PRI PRD PT PVEM PRS PMEP PC PSN PAS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PRS inv_mun_PMEP inv_mun_PC inv_mun_PSN inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PRS_r PMEP_r PC_r PSN_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PRS PMEP PC PSN PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2002
gen month ="July"

sort section

save Nayarit_Section_2002.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2005_No_LN.csv, clear

rename nombre_municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  total, replace

collapse (sum) pan -  prdptprs total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prdptprs    PRD_PT_PRS
rename  pc     PC
rename  pvem   PVEM

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLAN"
replace uniqueid=18003 if municipality =="AMATLAN DE CA?AS"
replace uniqueid=18020 if municipality =="BAHIA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="EL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLAN DEL RIO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUIZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"

egen valid = rowtotal(PAN PRI PVEM PC PRD_PT_PRS)

foreach var in PAN PRI PVEM PC PRD_PT_PRS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PVEM inv_mun_PC inv_mun_PRD_PT_PRS, gen(PAN_r PRI_r PVEM_r PC_r PRD_PT_PRS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PVEM PC PRD_PT_PRS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2005
gen month ="July"

sort section

save Nayarit_Section_2005.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2008.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename  lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan -  total, replace

collapse (sum) listanominal pan -  pripanal total, by (municipality section)

rename pan PAN
rename pripanal PRI_PANAL
rename prdpvem  PRD_PVEM
rename pt  PT
rename pcprs PC_PRS
rename pas PAS

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLAN"
replace uniqueid=18003 if municipality =="AMATLAN DE CA?AS"
replace uniqueid=18020 if municipality =="BAHIA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="EL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLAN DEL RIO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUIZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"


egen valid = rowtotal(PAN PT PAS PRD_PVEM PC_PRS PRI_PANAL)

foreach var in PAN PT PAS PRD_PVEM PC_PRS PRI_PANAL total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PT inv_mun_PAS inv_mun_PRD_PVEM inv_mun_PC_PRS inv_mun_PRI_PANAL, gen(PAN_r PT_r PAS_r PRD_PVEM_r PC_PRS_r PRI_PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PT PAS PRD_PVEM PC_PRS PRI_PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2008
gen month ="July"

sort section

save Nayarit_Section_2008.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2011.xlsx", sheet("Sheet1") firstrow clear

drop K L 
rename MUNICIPIO municipality
rename Section section

replace municipality = municipality[_n-1] if municipality =="" & municipality[_n-1]!=""

egen total = rowtotal(PAN PRD PRS PRI_PVEM_PANAL PT_PC CandidatosNoRegistrados VotosNulos)
drop if total ==. | total ==0
drop CandidatosNoRegistrados VotosNulos

replace municipality = trim(municipality)

collapse (sum) PAN - total, by (municipality section)

gen   uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLÁN"
replace uniqueid=18003 if municipality =="AMATLÁN DE CAÑAS"
replace uniqueid=18020 if municipality =="BAHÍA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="EL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLÁN DEL RÍO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUIZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARÍA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"

egen valid = rowtotal(PAN PRD PRS PRI_PVEM_PANAL PT_PC)

foreach var in PAN PRD PRS PRI_PVEM_PANAL PT_PC total valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD inv_mun_PRS inv_mun_PRI_PVEM_PANAL inv_mun_PT_PC, gen(PAN_r PRD_r PRS_r PRI_PVEM_PANAL_r PT_PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRD PRS PRI_PVEM_PANAL PT_PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2011
gen month ="July"

sort section

save Nayarit_Section_2011.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Nayarit_Section_1996.dta
append using Nayarit_Section_1999.dta
append using Nayarit_Section_2002.dta
append using Nayarit_Section_2005.dta
append using Nayarit_Section_2008.dta
append using Nayarit_Section_2011.dta

erase Nayarit_Section_1996.dta
erase Nayarit_Section_1999.dta
erase Nayarit_Section_2002.dta
erase Nayarit_Section_2005.dta
erase Nayarit_Section_2008.dta
erase Nayarit_Section_2011.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible PRS {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Nayarit_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Nayarit_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
