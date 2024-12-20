

capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Quintana Roo - 1999, 2002, 2005, 2008, 2010, 2013"

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Bacalar") firstrow clear

gen municipality="BACALAR"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen PAN_PRD= PAN + PRD
drop PAN PRD
gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PAN_PRD PRI PVEM MC PT)

foreach var in PAN_PRD PRI PVEM MC PT total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI inv_mun_PVEM inv_mun_MC inv_mun_PT , gen(PAN_PRD_r PRI_r PVEM_r MC_r PT_r )
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD PRI PVEM MC PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2013
gen month ="July"

sort section

save BACALAR_2013.dta, replace


**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Benito Juarez") firstrow clear

gen municipality="BENITO JUAREZ"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PRI_PVEM_PANAL -  listanominal , by (municipality section)

gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PRI_PVEM_PANAL PRD MC PT CI)

foreach var in PRI_PVEM_PANAL PRD MC PT CI total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI_PVEM_PANAL inv_mun_PRD inv_mun_MC inv_mun_PT inv_mun_CI, gen(PRI_PVEM_PANAL_r PRD_r MC_r PT_r CI_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI_PVEM_PANAL PRD MC PT CI {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save BENITO_JUAREZ_2013.dta, replace


**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Cozumel") firstrow clear

gen municipality="COZUMEL"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen PAN_PRD = PAN + PRD
drop PAN PRD
gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PAN_PRD PRI PVEM MC PT PANAL CI)

foreach var in PAN_PRD PRI PVEM MC PT PANAL CI total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI inv_mun_PVEM inv_mun_MC inv_mun_PT inv_mun_PANAL inv_mun_CI, gen(PAN_PRD_r PRI_r PVEM_r MC_r PT_r PANAL_r CI_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD PRI PVEM MC PT PANAL CI {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save COZUMEL_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Felipe Carrillo Puerto") firstrow clear

gen municipality="FELIPE CARRILLO PUERTO"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen PAN_PRD = PAN + PRD
drop PAN PRD
gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PAN_PRD PRI PT PVEM MC PANAL CI)

foreach var in PAN_PRD PRI PT PVEM MC PANAL CI total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI inv_mun_PVEM inv_mun_MC inv_mun_PT inv_mun_PANAL inv_mun_CI, gen(PAN_PRD_r PRI_r PVEM_r MC_r PT_r PANAL_r CI_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD PRI PT PVEM MC PANAL CI {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save FELIPE_CARRILLO_PUERTO_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Isla Mujeres") firstrow clear

gen municipality="ISLA MUJERES"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen PAN_PRD = PAN + PRD
drop PAN PRD
gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PRI_PVEM_PANAL MC PT PAN_PRD)

foreach var in PRI_PVEM_PANAL MC PT PAN_PRD total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI_PVEM_PANAL inv_mun_MC inv_mun_PT, gen(PAN_PRD_r PRI_PVEM_PANAL_r MC_r PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI_PVEM_PANAL MC PT PAN_PRD {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save ISLA_MUJERES_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Lazaro Cardenas") firstrow clear

gen municipality="LAZARO CARDENAS"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PAN PRI_PVEM_PANAL MC PT)

foreach var in PAN PRI_PVEM_PANAL MC PT total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PANAL inv_mun_MC inv_mun_PT, gen(PAN_r PRI_PVEM_PANAL_r MC_r PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL MC PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save LAZARO_CARDENAS_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Jose Maria Morelos") firstrow clear

gen municipality="JOSE MARIA MORELOS"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PRI -  listanominal , by (municipality section)

gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PRI PRD PVEM MC PT PANAL)

foreach var in PRI PRD PVEM MC PT PANAL total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_MC inv_mun_PT inv_mun_PANAL, gen(PRI_r PRD_r PVEM_r MC_r PT_r PANAL_r)
drop inv_mun_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PRD PVEM MC PT PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save JOSE_MARIA_MORELOS_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Othon P. Blanco") firstrow clear

gen municipality="OTHON P BLANCO"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PAN -  listanominal , by (municipality section)

gen PAN_PRD = PAN + PAN
drop PAN PRD
gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PAN_PRD PRI PVEM MC PT)

foreach var in PAN_PRD PRI PVEM MC PT total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI inv_mun_PVEM inv_mun_MC inv_mun_PT, gen(PAN_PRD_r PRI_r PVEM_r MC_r PT_r)
drop inv_mun_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD PRI PVEM MC PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r


gen year = 2013
gen month ="July"

sort section

save OTHON_P_BLANCO_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Solidaridad") firstrow clear

gen municipality="SOLIDARIDAD"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PRI -  listanominal , by (municipality section)

gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PRI PRD PVEM MC PT PANAL CI)

foreach var in PRI PRD PVEM MC PT PANAL CI total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_MC inv_mun_PT inv_mun_PANAL inv_mun_CI, gen(PRI_r PRD_r PVEM_r MC_r PT_r PANAL_r CI_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PRD PVEM MC PT PANAL CI {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save SOLIDARIDAD_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

import excel "CASILLAS_AYUNTAMIENTOS_2013.xlsx", sheet("Tulum") firstrow clear

gen municipality="TULUM"
capture rename SECCIN section
capture rename SECCIÓN section
drop if section==.
rename LISTANOMINAL listanominal
capture rename VOTACINTOTAL total
capture rename VOTACIÓNTOTAL total
drop if total==0  | total==.

collapse (sum)   PRI -  listanominal , by (municipality section)

gen turnout =  total/listanominal

drop VOTOSNULOS 

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
replace uniqueid=23010 if municipality =="BACALAR"

egen valid = rowtotal(PRI PRD PVEM MC PT PANAL)

foreach var in PRI PRD PVEM MC PT PANAL total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_MC inv_mun_PT inv_mun_PANAL, gen(PRI_r PRD_r PVEM_r MC_r PT_r PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PRI PRD PVEM MC PT PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}
drop *_r

gen year = 2013
gen month ="July"

sort section

save TULUM_2013.dta, replace

**************************************************************************************
**************************************************************************************
**************************************************************************************

clear
append using BACALAR_2013.dta
append using BENITO_JUAREZ_2013.dta
append using COZUMEL_2013.dta
append using FELIPE_CARRILLO_PUERTO_2013.dta
append using ISLA_MUJERES_2013.dta
append using JOSE_MARIA_MORELOS_2013.dta
append using LAZARO_CARDENAS_2013.dta
append using OTHON_P_BLANCO_2013.dta
append using SOLIDARIDAD_2013.dta
append using TULUM_2013.dta

erase BACALAR_2013.dta
erase BENITO_JUAREZ_2013.dta
erase COZUMEL_2013.dta
erase FELIPE_CARRILLO_PUERTO_2013.dta
erase ISLA_MUJERES_2013.dta
erase JOSE_MARIA_MORELOS_2013.dta
erase LAZARO_CARDENAS_2013.dta
erase OTHON_P_BLANCO_2013.dta
erase SOLIDARIDAD_2013.dta
erase TULUM_2013.dta

save Quintana_Roo_Section_2013.dta, replace
