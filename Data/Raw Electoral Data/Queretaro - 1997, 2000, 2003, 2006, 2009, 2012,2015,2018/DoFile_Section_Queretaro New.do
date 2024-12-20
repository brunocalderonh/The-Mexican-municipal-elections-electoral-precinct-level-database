
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Queretaro - 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Queretaro - 1997, 2000, 2003, 2006, 2009, 2012"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1997.csv, clear

rename municipio  municipality
rename seccin section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal -  pvem , replace

collapse (sum)  listanominal -  pvem , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"

egen valid = rowtotal(PAN PRI PRD PT PVEM)

foreach var in PAN PRI PRD PT PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
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

gen year = 1997
gen month ="July"

save Queretaro_Section_1997.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2000.csv, clear

rename municipio  municipality
rename seccin section
drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal -  psn , replace

collapse (sum)  listanominal -  psn , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
rename psn  PSN

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN)

foreach var in PAN PRI PRD PT PVEM PC PSN total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PSN {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2000
gen month ="July"

save Queretaro_Section_2000.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2003.csv, clear

rename municipio  municipality
rename seccin section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal -  psn , replace

collapse (sum)  listanominal -  psn , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
rename psn  PSN

g PRI_PVEM = PRI if PRI==PVEM
replace PRI = . if PRI_PVEM!=.
replace PVEM = . if PRI_PVEM!=.

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN)

foreach var in PAN PRI PRD PT PVEM PC PSN total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PSN {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2003
gen month ="July"

save Queretaro_Section_2003.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - listanominal  , replace

collapse (sum)  pan - pripvem total listanominal , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename convergencia   PC
rename pripvem  PRI_PVEM
rename panal  PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="Amealco de Bonfil"
replace uniqueid=22003 if municipality =="Arroyo Seco"
replace uniqueid=22004 if municipality =="Cadereyta de Montes"
replace uniqueid=22005 if municipality =="Colón"
replace uniqueid=22006 if municipality =="Corregidora"
replace uniqueid=22011 if municipality =="El Marqués"
replace uniqueid=22007 if municipality =="Ezequiel Montes"
replace uniqueid=22008 if municipality =="Huimilpan"
replace uniqueid=22009 if municipality =="Jalpan de Serra"
replace uniqueid=22010 if municipality =="Landa de Matamoros"
replace uniqueid=22012 if municipality =="Pedro Escobedo"
replace uniqueid=22013 if municipality =="Peñamiller"
replace uniqueid=22002 if municipality =="Pinal de Amoles"
replace uniqueid=22014 if municipality =="Querétaro"
replace uniqueid=22015 if municipality =="SAN JOAQUIN"
replace uniqueid=22016 if municipality =="San Juan del Río"
replace uniqueid=22017 if municipality =="Tequisquiapan"
replace uniqueid=22018 if municipality =="Tolimán"

egen valid = rowtotal(PAN PRI PRD PVEM PC PT PANAL PRI_PVEM)

foreach var in PAN PRI PRD PVEM PC PT PANAL PRI_PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PC inv_mun_PT inv_mun_PANAL inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_r PVEM_r PC_r PT_r PANAL_r PRI_PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PC PT PANAL PRI_PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2006
gen month ="July"

sort section

save Queretaro_Section_2006.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal - pvem total  , replace

collapse (sum) listanominal - pvem total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename convergencia   PC
rename psd  PSD
rename na  PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"

egen valid = rowtotal(PAN PRI PRD PC PANAL PSD PT PVEM)

foreach var in PAN PRI PRD PC PANAL PSD PT PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PC inv_mun_PANAL inv_mun_PSD inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_r PC_r PANAL_r PSD_r PT_r PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PC PANAL PSD PT PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2009
gen month ="July"

sort section

save Queretaro_Section_2009.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2012.csv, clear

drop if municipality=="" & section==.
drop if total==. | total==0 

collapse (sum) pan - prina total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename mc   PC
rename na  PANAL
rename pripvemna  PRI_PVEM_PANAL
rename pripvem  PRI_PVEM
rename prina  PRI_PANAL

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=22001 if municipality =="AMEALCO DE BONFIL"
replace uniqueid=22003 if municipality =="ARROYO SECO"
replace uniqueid=22004 if municipality =="CADEREYTA DE MONTES"
replace uniqueid=22005 if municipality =="COLÓN"
replace uniqueid=22006 if municipality =="CORREGIDORA"
replace uniqueid=22011 if municipality =="EL MARQUÉS"
replace uniqueid=22007 if municipality =="EZEQUIEL MONTES"
replace uniqueid=22008 if municipality =="HUIMILPAN"
replace uniqueid=22009 if municipality =="JALPAN DE SERRA"
replace uniqueid=22010 if municipality =="LANDA DE MATAMOROS"
replace uniqueid=22012 if municipality =="PEDRO ESCOBEDO"
replace uniqueid=22013 if municipality =="PEÑAMILLER"
replace uniqueid=22002 if municipality =="PINAL DE AMOLES"
replace uniqueid=22014 if municipality =="QUERÉTARO"
replace uniqueid=22015 if municipality =="SAN JOAQUÍN"
replace uniqueid=22016 if municipality =="SAN JUAN DEL RÍO"
replace uniqueid=22017 if municipality =="TEQUISQUIAPAN"
replace uniqueid=22018 if municipality =="TOLIMÁN"


egen valid = rowtotal(PAN PRI PRD PC PANAL PVEM PT PRI_PVEM_PANAL PRI_PVEM PRI_PANAL)

foreach var in PAN PRI PRD PC PANAL PVEM PT PRI_PVEM_PANAL PRI_PVEM PRI_PANAL total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PC inv_mun_PANAL inv_mun_PVEM inv_mun_PT inv_mun_PRI_PVEM_PANAL inv_mun_PRI_PVEM inv_mun_PRI_PANAL, gen(PAN_r PRI_r PRD_r PC_r PANAL_r PVEM_r PT_r PRI_PVEM_PANAL_r PRI_PVEM_r PRI_PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PC PANAL PVEM PT PRI_PVEM_PANAL PRI_PVEM PRI_PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2012
gen month ="July"

sort section

save Queretaro_Section_2012.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Queretaro_Section_1997.dta
append using Queretaro_Section_2000.dta
append using Queretaro_Section_2003.dta
append using Queretaro_Section_2006.dta
append using Queretaro_Section_2009.dta
append using Queretaro_Section_2012.dta

erase Queretaro_Section_1997.dta
erase Queretaro_Section_2000.dta
erase Queretaro_Section_2003.dta
erase Queretaro_Section_2006.dta
erase Queretaro_Section_2009.dta
erase Queretaro_Section_2012.dta

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

save Queretaro_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Queretaro_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
