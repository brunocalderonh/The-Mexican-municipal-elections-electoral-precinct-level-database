capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Colima - 1994, 1997, 2000, 2003, 2006, 2009, 2012"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1994_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring pan - total, replace

collapse (sum) pan - total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pfcrn  PartCardenista
rename  ptc     PT
rename  prt    PRT
rename  pps    PPS
rename  pdm    PD

drop   nulos  noreg 

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN PRI PPS PRD PartCardenista PD PRT PT)

foreach var in PAN PRI PPS PRD PartCardenista PD PRT PT total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 6
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

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PPS inv_mun_PRD inv_mun_PartCardenista inv_mun_PD inv_mun_PRT inv_mun_PT, gen(PAN_r PRI_r PPS_r PRD_r PartCardenista_r PD_r PRT_r PT_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PartCardenista" if PartCardenista_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PD" if PD_r==1
replace winner = "PRT" if PRT_r==1
drop *_r

gen year =1994
gen month ="August"

sort section

save Colima_Section_1994.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_1997.csv, clear

rename municipio  municipality
rename  casillas section
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal  - total, replace

collapse (sum) listanominal  - total, by (municipality section)
 
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     PartCardenista
rename  pt     PT
rename  pvem   PVEM
rename  pps    PPS
rename  pdm    PD

gen turnout =  total/listanominal 

drop    noreg nulos 

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN PRI PRD PartCardenista PT PVEM PPS PD)

foreach var in PAN PRI PRD PartCardenista PT PVEM PPS PD total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartCardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PD, gen(PAN_r PRI_r PRD_r PartCardenista_r PT_r PVEM_r PPS_r PD_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PartCardenista" if PartCardenista_r==1
replace winner = "PPS" if PPS_r==1
replace winner = "PD" if PD_r==1

drop *_r

gen year =1997
gen month ="July"

save Colima_Section_1997.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2000_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring total - nulos, replace

collapse (sum) total - nulos, by (municipality section)

rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pvem   PVEM
rename  ds     PDS
rename  adc    ADC
rename  prdpan PAN_PRD

drop    noreg nulos 

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal( PAN PRI PRD PVEM PDS ADC PAN_PRD)

foreach var in  PAN PRI PRD PVEM PDS ADC PAN_PRD total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 6
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==7 & year==2000
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PVEM inv_mun_PDS inv_mun_ADC inv_mun_PAN_PRD, gen(PAN_r PRI_r PRD_r PVEM_r PDS_r ADC_r PAN_PRD_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PDS" if PDS_r==1
replace winner = "ADC" if ADC_r==1
replace winner = "PAN_PRD" if PAN_PRD_r==1

drop *_r

gen year =2000
gen month ="July"

save Colima_Section_2000.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2003.csv, clear

rename  nombre_municipio  municipality
rename  seccion section
drop if municipality=="" & section==.
drop if total==. | total==0
 
destring listanominal pan - fuerzaciudadana total, replace

collapse (sum) listanominal pan - fuerzaciudadana total, by (municipality section)
 
rename pan PAN
rename pri PRI
rename prd  PRD
rename pt  PT
rename pvem  PVEM
rename pc  PC
rename psn PSN 
rename pas  PAS
rename adc  ADC
rename mexicoposible  MexicoPosible
rename fuerzaciudadana FC
 
gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN PAS ADC MexicoPosible FC)

foreach var in PAN PRI PRD PT PVEM PC PSN PAS ADC MexicoPosible FC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_ADC inv_mun_MexicoPosible inv_mun_FC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r PAS_r ADC_r MexicoPosible_r FC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "ADC" if ADC_r==1
replace winner = "MexicoPosible" if MexicoPosible_r==1
replace winner = "FC" if FC_r==1
drop *_r

gen year =2003
gen month ="July"

sort section

save Colima_Section_2003.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename  nombre_municipio  municipality
rename  seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal pan - coalicion total, replace

collapse (sum) listanominal pan - coalicion total, by (municipality section)

rename pan PAN
rename pripvem PRI_PVEM
rename prdadc  PRD_ADC
rename ptpc  PT_PC
rename coalicion  PAS

gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN PRI_PVEM PRD_ADC PT_PC PAS)

foreach var in PAN PRI_PVEM PRD_ADC PT_PC PAS total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD_ADC inv_mun_PT_PC inv_mun_PAS, gen(PAN_r PRI_PVEM_r PRD_ADC_r PT_PC_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1
replace winner = "PRD_ADC" if PRD_ADC_r==1
replace winner = "PT_PC" if PT_PC_r==1
replace winner = "PAS" if PAS_r==1
drop *_r

gen year =2006
gen month ="July"

sort section

save Colima_Section_2006.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename  nombre_municipio  municipality
rename  seccion section
drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal  panadc - prdpsd total, replace

collapse (sum) listanominal panadc - prdpsd total, by (municipality section)

replace  pripanal= pri + panal + pripanal
drop pri panal
replace prdpsd = prd + psd + prdpsd
drop prd psd
 
rename panadc PAN_ADC
rename pripanal PRI_PANAL
rename prdpsd  PRD_PSD
rename pt  PT
rename pv  PVEM
rename pc  PC

gen turnout =  total/listanominal

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN_ADC PT PVEM PC PRI_PANAL PRD_PSD)

foreach var in PAN_ADC PT PVEM PC PRI_PANAL PRD_PSD total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_ADC inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PRI_PANAL inv_mun_PRD_PSD, gen(PAN_ADC_r PT_r PVEM_r PC_r PRI_PANAL_r PRD_PSD_r)
drop inv_mun_*

gen winner = "PAN_ADC" if PAN_ADC_r==1  
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1
replace winner = "PRD_PSD" if PRD_PSD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
drop *_r

gen year =2009
gen month ="July"

sort section

save Colima_Section_2009.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

use "CASILLAS_AYUNTAMIENTOS_2012\Ayu_Seccion_2012.dta", clear

drop if municipality=="" & section==.
rename Total total
drop if total==. | total==0

collapse (sum) PAN - ADC total, by (municipality section)

gen     uniqueid= 0
replace uniqueid=6001 if municipality=="ARMERIA"
replace uniqueid=6002 if municipality=="COLIMA"
replace uniqueid=6003 if municipality=="COMALA"
replace uniqueid=6004 if municipality=="COQUIMATLAN"
replace uniqueid=6005 if municipality=="CUAUHTEMOC"
replace uniqueid=6006 if municipality=="IXTLAHUACAN"
replace uniqueid=6007 if municipality=="MANZANILLO"
replace uniqueid=6008 if municipality=="MINATITLAN"
replace uniqueid=6009 if municipality=="TECOMAN"
replace uniqueid=6010 if municipality=="VILLA DE ALVAREZ"

egen valid = rowtotal(PAN PRI_PANAL PRD PT PVEM PC ADC)

foreach var in PAN PRI_PANAL PRD PT PVEM PC ADC total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 6
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista day)
keep if month==7 & year==2012 & day==1
drop if _merge==2
drop _merge ed seccion year month day
rename lista listanominal

gen turnout =  total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PANAL inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_ADC, gen(PAN_r PRI_PANAL_r PRD_r PT_r PVEM_r PC_r ADC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1
replace winner = "PRD" if PRD_r==1
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "ADC" if PC_r==1
drop *_r

gen year =2012
gen month ="July"

sort section

save Colima_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

use Colima_Section_1994.dta, clear
append using Colima_Section_1997.dta
append using Colima_Section_2000.dta
append using Colima_Section_2003.dta
append using Colima_Section_2006.dta
append using Colima_Section_2009.dta
append using Colima_Section_2012.dta

erase Colima_Section_1994.dta
erase Colima_Section_1997.dta
erase Colima_Section_2000.dta
erase Colima_Section_2003.dta
erase Colima_Section_2006.dta
erase Colima_Section_2009.dta
erase Colima_Section_2012.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL ADC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

saveold "..\Colima_ALL.dta", replace version(12)
