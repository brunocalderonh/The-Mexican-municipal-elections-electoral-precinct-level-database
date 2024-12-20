
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013"


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1999.csv, clear

rename municipio  municipality
rename seccion section

drop if section==. | total==0

destring pan - total , replace

collapse (sum)  pan - listanominal , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM

drop   noregistrados nulos   

gen   uniqueid= 0
replace uniqueid=23005 if municipality =="BENITO JUÁREZ"
replace uniqueid=23001 if municipality =="COZUMEL"
replace uniqueid=23002 if municipality =="FELIPE CARRILLO PUERTO"
replace uniqueid=23003 if municipality =="ISLA MUJERES"
replace uniqueid=23006 if municipality =="JOSÉ MARÍA MORELOS"
replace uniqueid=23007 if municipality =="LÁZARO CÁRDENAS"
replace uniqueid=23004 if municipality =="OTHÓN P. BLANCO"
replace uniqueid=23008 if municipality =="SOLIDARIDAD"

gen turnout =  total/listanominal

egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM , gen(PAN_r PRI_r PRD_r PT_r PVEM_r )
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

gen year = 1999
gen month ="February"

sort section

save Quintana_Roo_Section_1999.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Listanominal2002.xlsx", sheet("Sheet1") firstrow clear
sort section
save Listanominal2002.dta, replace


import excel "Listanominal2005.xlsx", sheet("Sheet1") firstrow clear
sort section
save Listanominal2005.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2002_No_Municipios.csv, clear

rename seccion section
rename vtotal total

drop if section==. | total==0

destring pan - total , replace

* collapse (sum)  pan - total , by (municipality section)
collapse (sum)  pan - total , by (section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename cdem CDEM
rename psn PSN
rename pas PAS
   
drop   noregistrados nulos   

sort section

merge section using Quintana_Roo_Section_to_Merge_Municipalities_2002.dta
drop if _merge==2
drop _merge

replace municipality ="BENITO JUÁREZ" if section ==90 & municipality ==""
replace uniqueid=23005 if section ==90 & uniqueid ==.

replace municipality ="BENITO JUÁREZ" if section ==150 & municipality ==""
replace uniqueid=23005 if section ==150 & uniqueid ==.

sort section
merge section using Listanominal2002.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

egen valid = rowtotal(PAN PRI PRD PT PVEM CDEM PSN PAS)

foreach var in PAN PRI PRD PT PVEM CDEM PSN PAS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_CDEM inv_mun_PSN inv_mun_PAS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r CDEM_r PSN_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM CDEM PSN PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2002
gen month ="February"

sort section

save Quintana_Roo_Section_2002.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2005_No_Municipios.csv, clear

rename seccion section
rename  votostotales total

drop if section==. | total==0 

destring  panpc - total , replace

collapse (sum)   panpc - total , by (section)

rename panpc  PAN_PC
rename pripvem  PRI_PVEM
rename prdpt  PRD_PT
   
drop  votosnulos votosvlidos 

sort section

merge section using Quintana_Roo_Section_to_Merge_Municipalities_2002.dta
drop if _merge==2
drop _merge

replace municipality ="BENITO JUÁREZ" if section ==90 & municipality ==""
replace uniqueid=23005 if section ==90 & uniqueid ==.

replace municipality ="BENITO JUÁREZ" if section ==150 & municipality ==""
replace uniqueid=23005 if section ==150 & uniqueid ==.

sort section
merge section using Listanominal2005.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

egen valid = rowtotal(PAN_PC PRI_PVEM PRD_PT)

foreach var in PAN_PC PRI_PVEM PRD_PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

rowranks inv_mun_PAN_PC inv_mun_PRI_PVEM inv_mun_PRD_PT, gen(PAN_PC_r PRI_PVEM_r PRD_PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PC PRI_PVEM PRD_PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2005
gen month ="February"

sort section

save Quintana_Roo_Section_2005.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2008.csv, clear

rename nombre_municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==0 

destring  listanominal  nulos -  total , replace

collapse (sum)   listanominal  nulos -  total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prdptpc  PRD_PT_PC
rename panal   PANAL
rename pvem PVEM
rename pas PAS
rename pripvem PRI_PVEM

gen turnout =  total/listanominal

drop   noregistrados nulos  

gen   uniqueid= 0
replace uniqueid=23005 if municipality =="BENITO JUAREZ"
replace uniqueid=23001 if municipality =="COZUMEL"
replace uniqueid=23002 if municipality =="FELIPE CARRILLO PUERTO"
replace uniqueid=23003 if municipality =="ISLA MUJERES"
replace uniqueid=23006 if municipality =="JOSE MARIA MORELOS"
replace uniqueid=23007 if municipality =="LAZARO CARDENAS"
replace uniqueid=23004 if municipality =="OTHON P BLANCO"
replace uniqueid=23008 if municipality =="SOLIDARIDAD"

egen valid = rowtotal(PAN PRI PRD_PT_PC PVEM PANAL PAS PRI_PVEM)

foreach var in PAN PRI PRD_PT_PC PVEM PANAL PAS PRI_PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT_PC inv_mun_PVEM inv_mun_PANAL inv_mun_PAS inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_PT_PC_r PVEM_r PANAL_r PAS_r PRI_PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT_PC PVEM PANAL PAS PRI_PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2008
gen month ="February"

sort section

save Quintana_Roo_Section_2008.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==0 

destring  listanominal  -  total , replace

collapse (sum)   listanominal -  total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename panal   PANAL
rename pvem PVEM
rename pt PT
rename panprdptconvergencia PAN_PRD_PT_PC
rename pripvempanal PRI_PVEM_PANAL

gen turnout =  total/listanominal

drop nulos 

gen   uniqueid= 0
replace uniqueid=23005 if municipality =="BENITO JUAREZ"
replace uniqueid=23001 if municipality =="COZUMEL"
replace uniqueid=23002 if municipality =="FELIPE CARRILLO PUERTO"
replace uniqueid=23003 if municipality =="ISLA MUJERES"
replace uniqueid=23006 if municipality =="JOSE MARIA MORELOS"
replace uniqueid=23007 if municipality =="LAZARO CARDENAS"
replace uniqueid=23004 if municipality =="OTHON P BLANCO"
replace uniqueid=23008 if municipality =="SOLIDARIDAD"
replace uniqueid=23009 if municipality =="TULUM"

egen valid = rowtotal(PAN PRI PRD PVEM PT PANAL PAN_PRD_PT_PC PRI_PVEM_PANAL)

foreach var in PAN PRI PRD PVEM PT PANAL PAN_PRD_PT_PC PRI_PVEM_PANAL total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PT inv_mun_PANAL inv_mun_PAN_PRD_PT_PC inv_mun_PRI_PVEM_PANAL, gen(PAN_r PRI_r PRD_r PVEM_r PT_r PANAL_r PAN_PRD_PT_PC_r PRI_PVEM_PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT PANAL PAN_PRD_PT_PC PRI_PVEM_PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2010
gen month ="February"

sort section

save Quintana_Roo_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************



**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Quintana_Roo_Section_1999.dta, clear
append using Quintana_Roo_Section_2002.dta
append using Quintana_Roo_Section_2005.dta
append using Quintana_Roo_Section_2008.dta
append using Quintana_Roo_Section_2010.dta
append using Quintana_Roo_Section_2013.dta

erase Quintana_Roo_Section_1999.dta
erase Quintana_Roo_Section_2002.dta
erase Quintana_Roo_Section_2005.dta
erase Quintana_Roo_Section_2008.dta
erase Quintana_Roo_Section_2010.dta
erase Quintana_Roo_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

tab winner winner_counter

save Quintana_Roo_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Quintana_Roo_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

