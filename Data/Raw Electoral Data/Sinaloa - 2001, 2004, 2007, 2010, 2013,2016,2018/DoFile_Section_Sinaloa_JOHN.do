
clear all
set mem 1g

capture cd "C:\Users\jmarshall\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Sinaloa - 2001, 2004, 2007, 2010, 2013"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Sinaloa - 2001, 2004, 2007, 2010, 2013"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.

destring pan - ptcdppnpbs , replace

**************************************************************************

sum pan prd  pvem panprdpvem if panprdpvem!=. 
replace panprdpvem  = panprdpvem  + pan + prd + pvem  if panprdpvem!=. 
replace pan   = 0 if panprdpvem!=. 
replace prd   = 0 if panprdpvem!=. 
replace pvem  = 0 if panprdpvem!=. 

sum pan pvem panpvem if panpvem!=. 
replace panpvem  = panpvem  + pan + pvem  if panpvem!=. 
replace pan   = 0 if panpvem!=. 
replace pvem  = 0 if panpvem!=. 

sum prdpt prd pt if prdpt!=.
replace prdpt = prdpt + prd + pt if prdpt!=.
replace prd = 0 if prdpt!=.
replace pt = 0 if prdpt!=.

sum  ptpsn pt psn if  ptpsn !=.
replace ptpsn = ptpsn + pt + psn if  ptpsn !=.
replace pt  = 0 if  ptpsn !=.
replace psn = 0 if  ptpsn !=.

sum psnpbs psn pbs if  psnpbs !=.
replace psnpbs = psnpbs + psn + pbs if  psnpbs !=.
replace psn =0 if  psnpbs !=.
replace pbs =0 if  psnpbs !=.
 
sum ptpascdppnpsn ptpas cdppn psn if ptpascdppnpsn!=.
replace ptpascdppnpsn = ptpascdppnpsn + ptpas + cdppn + psn  if ptpascdppnpsn!=.
replace ptpas  = 0   if ptpascdppnpsn!=.
replace cdppn  = 0   if ptpascdppnpsn!=.
replace psn  = 0 if ptpascdppnpsn!=.
drop ptpas

sum  prdcdppn prd cdppn if  prdcdppn !=.
replace prdcdppn = prdcdppn + prd + cdppn if  prdcdppn !=.
replace prd   =0 if  prdcdppn !=.
replace cdppn =0 if  prdcdppn !=.

sum  panprd pan prd if  panprd !=.
replace panprd = panprd + pan + prd if  panprd !=.
replace pan = 0 if  panprd !=.
replace prd = 0 if  panprd !=.

sum  ptpbs pt pbs if ptpbs!=.
replace  ptpbs = ptpbs  + pt + pbs if ptpbs!=.
replace  pt  =0 if ptpbs!=.
replace  pbs =0 if ptpbs!=.

sum  panprdptpvem  pan prd pt pvem  if panprdptpvem !=.
replace panprdptpvem  = panprdptpvem + pan + prd + pt + pvem  if panprdptpvem !=.
replace pan   =0 if panprdptpvem !=.
replace prd   =0 if panprdptpvem !=.
replace pt    =0 if panprdptpvem !=.
replace pvem  =0 if panprdptpvem !=.

sum  ptcdppnpsn  pt cdppn psn  if ptcdppnpsn !=.
replace ptcdppnpsn = ptcdppnpsn + pt + cdppn + psn  if ptcdppnpsn !=.
replace pt     =0  if ptcdppnpsn !=.
replace cdppn  =0  if ptcdppnpsn !=.
replace psn    =0 if ptcdppnpsn !=.

sum  prdpvem  prd pvem  if prdpvem !=.
replace prdpvem = prdpvem + prd + pvem  if prdpvem !=.
replace prd   = 0 if prdpvem !=.
replace pvem  = 0 if prdpvem !=.

sum  ptcdppnpbs  pt cdppn pbs if ptcdppnpbs!=.
replace ptcdppnpbs = ptcdppnpbs + pt + cdppn + pbs if ptcdppnpbs!=.
replace pt = 0 if ptcdppnpbs!=.
replace cdppn = 0 if ptcdppnpbs!=.
replace pbs = 0 if ptcdppnpbs!=.

**************************************************************************

collapse (sum)  pan - ptcdppnpbs , by (municipality section)

egen total = rowtotal(pan pri prd pt pvem cdppn psn pas pbs  noregistrados nulos panprdpvem panpvem prdpt ptpsn psnpbs ptpascdppnpsn prdcdppn panprd ptpbs panprdptpvem ptcdppnpsn prdpvem ptcdppnpbs )
drop if total==0 

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename cdppn PC 
rename psn PSN
rename pas PAS
* PBS stands for Partido Barzonista Sinaloense
rename pbs PBS
rename panprdpvem PAN_PRD_PVEM 
rename panpvem  PAN_PVEM
rename prdpt  PRD_PT
rename ptpsn  PT_PSN
rename psnpbs  PSN_PBS
rename ptpascdppnpsn  PT_PAS_PC_PSN
rename prdcdppn  PRD_PC
rename panprd  PAN_PRD
rename ptpbs  PT_PBS
rename panprdptpvem  PAN_PRD_PT_PVEM
rename ptcdppnpsn  PT_PC_PSN
rename prdpvem  PRD_PVEM
rename ptcdppnpbs PT_PC_PBS

*sort section
*merge section using Ayu_Seccion_LN_2001.dta
*drop if _merge==2
*drop _merge

g ed = 25
g seccion = section
capture merge 1:m ed seccion using "C:\Users\Horacio\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
capture merge 1:m ed seccion using "C:\Users\jmarshall\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
keep if month==9 & year==2001
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

gen turnout =  total/listanominal

drop   noregistrados nulos  

gen   uniqueid= 0
replace uniqueid=25001 if municipality =="AHOME"
replace uniqueid=25002 if municipality =="ANGOSTURA"
replace uniqueid=25003 if municipality =="BADIRAGUATO"
replace uniqueid=25007 if municipality =="CHOIX"
replace uniqueid=25004 if municipality =="CONCORDIA"
replace uniqueid=25005 if municipality =="COSALA"
replace uniqueid=25006 if municipality =="CULIACAN"
replace uniqueid=25010 if municipality =="EL FUERTE"
replace uniqueid=25008 if municipality =="ELOTA"
replace uniqueid=25009 if municipality =="ESCUINAPA"
replace uniqueid=25011 if municipality =="GUASAVE"
replace uniqueid=25012 if municipality =="MAZATLAN"
replace uniqueid=25013 if municipality =="MOCORITO"
replace uniqueid=25018 if municipality =="NAVOLATO"
replace uniqueid=25014 if municipality =="ROSARIO"
replace uniqueid=25015 if municipality =="SALVADOR ALVARADO"
replace uniqueid=25016 if municipality =="SAN IGNACIO"
replace uniqueid=25017 if municipality =="SINALOA"


egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN PAS PBS PAN_PRD_PVEM PAN_PVEM PRD_PT PT_PSN PSN_PBS PT_PAS_PC_PSN PRD_PC PAN_PRD PT_PBS PAN_PRD_PT_PVEM PT_PC_PSN PRD_PVEM PT_PC_PBS)

foreach var in PAN PRI PRD PT PVEM PC PSN PAS PBS PAN_PRD_PVEM PAN_PVEM PRD_PT PT_PSN PSN_PBS PT_PAS_PC_PSN PRD_PC PAN_PRD PT_PBS PAN_PRD_PT_PVEM PT_PC_PSN PRD_PVEM PT_PC_PBS total valid listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PBS inv_mun_PAN_PRD_PVEM inv_mun_PAN_PVEM inv_mun_PRD_PT inv_mun_PT_PSN inv_mun_PSN_PBS inv_mun_PT_PAS_PC_PSN inv_mun_PRD_PC inv_mun_PAN_PRD inv_mun_PT_PBS inv_mun_PAN_PRD_PT_PVEM inv_mun_PT_PC_PSN inv_mun_PRD_PVEM inv_mun_PT_PC_PBS, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r PAS_r PBS_r PAN_PRD_PVEM_r PAN_PVEM_r PRD_PT_r PT_PSN_r PSN_PBS_r PT_PAS_PC_PSN_r PRD_PC_r PAN_PRD_r PT_PBS_r PAN_PRD_PT_PVEM_r PT_PC_PSN_r PRD_PVEM_r PT_PC_PBS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PBS" if PBS_r==1
replace winner = "PSN" if PSN_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PAN_PRD" if PAN_PRD_r==1 
replace winner = "PAN_PRD_PVEM" if PAN_PRD_PVEM_r==1 
replace winner = "PAN_PRD_PT_PVEM" if PAN_PRD_PT_PVEM_r==1 
replace winner = "PAN_PVEM" if PAN_PVEM_r==1 
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRD_PC" if PRD_PC_r==1
replace winner = "PRD_PVEM" if PRD_PVEM_r==1
replace winner = "PT_PC_PSN" if PT_PC_PSN_r==1
replace winner = "PT_PC_PBS" if PT_PC_PBS_r==1
replace winner = "PT_PSN" if PT_PSN_r==1
replace winner = "PT_PBS" if PT_PBS_r==1
replace winner = "PSN_PBS" if PSN_PBS_r==1
replace winner = "PT_PAS_PC_PSN" if PT_PAS_PC_PSN_r==1
drop *_r

gen year = 2001
gen month ="November"

sort section

save Sinaloa_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Ayu_Seccion_LN_2004.xlsx", sheet("Sheet1") firstrow clear

capture rename SECCIÓN SECCIN
drop if SECCIN==.
rename SECCIN section
rename LISTANOMINAL listanominal 

collapse (sum) listanominal, by(section)

sort section

save Ayu_Seccion_LN_2004.dta, replace 

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.

destring pan -  prdptpc , replace

**************************************************************************

sum prdpt prd pt if prdpt!=.
replace prdpt = prdpt + prd + pt if prdpt!=.
replace prd = 0 if prdpt!=.
replace pt  = 0 if prdpt!=.

sum prdptpbs prd pt pbs if prdptpbs !=.
replace prdptpbs = prdptpbs + prd + pt + pbs if prdptpbs !=.
replace prd =0   if prdptpbs !=.
replace pt  =0   if prdptpbs !=.
replace pbs =0   if prdptpbs !=.

sum prdpbs prd pbs if prdpbs!=.
replace prdpbs = prdpbs + prd + pbs if prdpbs!=.
replace prd = 0 if prdpbs!=.
replace pbs = 0 if prdpbs!=.

sum panprd pan prd if panprd!=. 
replace panprd = panprd + pan + prd if panprd!=. 
replace pan = 0 if panprd!=. 
replace prd = 0 if panprd!=. 

sum pripvempbs pri pvem pbs if pripvempbs!=.

sum prdptpc prd pt pc if prdptpc!=.
replace prdptpc = prdptpc + prd + pt + pc if prdptpc!=.
replace prd = 0 if prdptpc!=. 
replace pt = 0 if prdptpc!=. 
replace pc = 0 if prdptpc!=. 

**************************************************************************

collapse (sum)  pan -  prdptpc , by (municipality section)

egen total = rowtotal(pan  pri  prd  pt  pvem  pc  pbs  noregistrados  nulos  prdpt  prdptpbs  prdpbs  panprd  pripvempbs  prdptpc)

drop if total==0 

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc PC 
rename pbs PBS
rename panprd PAN_PRD 
rename prdpt  PRD_PT
rename prdptpc PRD_PT_PC
rename prdptpbs    PRD_PT_PBS
rename prdpbs      PRD_PBS 
rename pripvempbs  PRI_PVEM_PBS

sort section
merge section using Ayu_Seccion_LN_2004.dta
drop if _merge==2
drop _merge

gen turnout =  total/listanominal

drop   noregistrados nulos  

gen   uniqueid= 0
replace uniqueid=25001 if municipality =="AHOME"
replace uniqueid=25002 if municipality =="ANGOSTURA"
replace uniqueid=25003 if municipality =="BADIRAGUATO"
replace uniqueid=25007 if municipality =="CHOIX"
replace uniqueid=25004 if municipality =="CONCORDIA"
replace uniqueid=25005 if municipality =="COSALA"
replace uniqueid=25006 if municipality =="CULIACAN"
replace uniqueid=25010 if municipality =="EL FUERTE"
replace uniqueid=25008 if municipality =="ELOTA"
replace uniqueid=25009 if municipality =="ESCUINAPA"
replace uniqueid=25011 if municipality =="GUASAVE"
replace uniqueid=25012 if municipality =="MAZATLAN"
replace uniqueid=25013 if municipality =="MOCORITO"
replace uniqueid=25018 if municipality =="NAVOLATO"
replace uniqueid=25014 if municipality =="EL ROSARIO"
replace uniqueid=25015 if municipality =="SALVADOR ALVARADO"
replace uniqueid=25016 if municipality =="SAN IGNACIO"
replace uniqueid=25017 if municipality =="SINALOA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PBS PRD_PT PRD_PT_PBS PRD_PBS PAN_PRD PRI_PVEM_PBS PRD_PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PBS PRD_PT PRD_PT_PBS PRD_PBS PAN_PRD PRI_PVEM_PBS PRD_PT_PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PBS inv_mun_PRD_PT inv_mun_PRD_PT_PBS inv_mun_PRD_PBS inv_mun_PAN_PRD inv_mun_PRI_PVEM_PBS inv_mun_PRD_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PBS_r PRD_PT_r PRD_PT_PBS_r PRD_PBS_r PAN_PRD_r PRI_PVEM_PBS_r PRD_PT_PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PBS" if PBS_r==1
replace winner = "PAN_PRD" if PAN_PRD_r==1 
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRD_PT_PC" if PRD_PT_PC_r==1
replace winner = "PRD_PT_PBS" if PRD_PT_PBS_r==1
replace winner = "PRD_PBS" if PRD_PBS_r==1
replace winner = "PRI_PVEM_PBS" if PRI_PVEM_PBS_r==1
drop *_r

gen year = 2004
gen month ="November"

sort section

save Sinaloa_Section_2004.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Ayu_Seccion_LN_2007.xlsx", sheet("Sheet1") firstrow clear

capture rename SECCIÓN SECCIN
drop if SECCIN==.
rename SECCIN section
rename LISTANOMINAL listanominal 

g missing = listanominal==0

collapse (sum) listanominal missing, by(section)

g ed = 25
g seccion = section
capture merge 1:m ed seccion using "C:\Users\Horacio\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
capture merge 1:m ed seccion using "C:\Users\jmarshall\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
keep if month==9 & year==2007
drop if _merge==2
drop _merge ed seccion year month

replace listanominal = lista if missing>=1 & lista>listanominal
drop missing lista

sort section

save Ayu_Seccion_LN_2007.dta, replace 

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.

destring pan -   ptpas , replace

**************************************************************************

sum pripanalprd pripanal prd if pripanalprd !=.
replace pripanalprd = pripanalprd + pripanal + prd if pripanalprd !=.
replace pripanal = 0 if pripanalprd !=.
replace prd = 0 if pripanalprd !=.

sum prdptpc prd pt pc  if prdptpc !=.
replace prdptpc = prdptpc + prd + pt + pc  if prdptpc !=.
replace prd  = 0 if prdptpc !=.
replace pt  = 0 if prdptpc !=.
replace pc  = 0 if prdptpc !=.

sum prdpt prd pt if prdpt!=.
replace prdpt = prdpt + prd + pt if prdpt!=.
replace prd = 0 if prdpt!=.
replace pt = 0 if prdpt!=.

sum pripanalpc pripanal pc if pripanalpc!=. 
replace pripanalpc = pripanalpc + pripanal + pc if pripanalpc!=. 
replace pripanal  =0 if pripanalpc!=. 
replace pc =0 if pripanalpc!=. 

sum ptpas pt pas if ptpas!=.
replace ptpas = ptpas + pt + pas if ptpas!=.
replace pt  =0 if ptpas!=.
replace pas =0 if ptpas!=.

**************************************************************************

collapse (sum)  pan -   ptpas , by (municipality section)

egen total = rowtotal(pan  pripanal  prd  pt  pvem  pc  pas  noregistrados  nulos  pripanalprd  prdptpc  prdpt  pripanalpc  ptpas  prdpt)

drop if total==0 

rename pan  PAN
rename pripanal  PRI_PANAL
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc PC 
rename pas PAS
rename pripanalprd PRI_PRD_PANAL
rename pripanalpc PRI_PC_PANAL
rename prdptpc PRD_PT_PC
rename prdpt  PRD_PT
rename ptpas PT_PAS

sort section
merge section using Ayu_Seccion_LN_2007.dta
drop if _merge ==2
drop _merge 

gen turnout =  total/listanominal

drop   noregistrados nulos   

gen   uniqueid= 0
replace uniqueid=25001 if municipality =="AHOME"
replace uniqueid=25002 if municipality =="ANGOSTURA"
replace uniqueid=25003 if municipality =="BADIRAGUATO"
replace uniqueid=25007 if municipality =="CHOIX"
replace uniqueid=25004 if municipality =="CONCORDIA"
replace uniqueid=25005 if municipality =="COSALA"
replace uniqueid=25006 if municipality =="CULIACAN"
replace uniqueid=25010 if municipality =="EL FUERTE"
replace uniqueid=25008 if municipality =="ELOTA"
replace uniqueid=25009 if municipality =="ESCUINAPA"
replace uniqueid=25011 if municipality =="GUASAVE"
replace uniqueid=25012 if municipality =="MAZATL�N"
replace uniqueid=25012 if strpos(municipality, "MAZATL")>0
replace uniqueid=25013 if municipality =="MOCORITO"
replace uniqueid=25018 if municipality =="NAVOLATO"
replace uniqueid=25014 if municipality =="ROSARIO"
replace uniqueid=25015 if municipality =="SALVADOR ALVARADO"
replace uniqueid=25016 if municipality =="SAN IGNACIO"
replace uniqueid=25017 if municipality =="SINALOA"

egen valid = rowtotal(PAN PRI_PANAL PRD PT PVEM PC PAS PRI_PRD_PANAL PRD_PT_PC PRD_PT PRI_PC_PANAL PT_PAS)

foreach var in PAN PRI_PANAL PRD PT PVEM PC PAS PRI_PRD_PANAL PRD_PT_PC PRD_PT PRI_PC_PANAL PT_PAS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PANAL inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PAS inv_mun_PRI_PRD_PANAL inv_mun_PRD_PT_PC inv_mun_PRD_PT inv_mun_PRI_PC_PANAL inv_mun_PT_PAS, gen(PAN_r PRI_PANAL_r PRD_r PT_r PVEM_r PC_r PAS_r PRI_PRD_PANAL_r PRD_PT_PC_r PRD_PT_r PRI_PC_PANAL_r PT_PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1 
replace winner = "PRD" if PRD_r==1 
replace winner = "PT" if PT_r==1
replace winner = "PVEM" if PVEM_r==1
replace winner = "PC" if PC_r==1
replace winner = "PAS" if PAS_r==1
replace winner = "PRI_PRD_PANAL" if PRI_PRD_PANAL_r ==1 
replace winner = "PRD_PT_PC" if PRD_PT_PC_r==1
replace winner = "PRD_PT" if PRD_PT_r==1
replace winner = "PRI_PC_PANAL" if PRI_PC_PANAL_r ==1 
replace winner = "PT_PAS" if PT_PAS_r==1 
drop *_r

gen year = 2007
gen month ="October"

sort section

save Sinaloa_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal  nulos -   total , replace

g missing = listanominal==0

collapse (sum) missing listanominal  nulos -   total , by (municipality section)

g ed = 25
g seccion = section
capture merge 1:m ed seccion using "C:\Users\Horacio\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
capture merge 1:m ed seccion using "C:\Users\jmarshall\Dropbox\Turismo Electoral\all_months_years.dta", keepusing(month year lista)
keep if month==6 & year==2010
drop if _merge==2
drop _merge ed seccion year month

replace listanominal = lista if missing>=1 & lista>listanominal
drop missing lista

rename panprdptpc   PAN_PRD_PT_PC
rename pripvempanal  PRI_PVEM_PANAL

gen turnout =  total/listanominal

drop   noregistrados nulos 

gen   uniqueid= 0
replace uniqueid=25001 if municipality =="AHOME"
replace uniqueid=25002 if municipality =="ANGOSTURA"
replace uniqueid=25003 if municipality =="BADIRAGUATO"
replace uniqueid=25007 if municipality =="CHOIX"
replace uniqueid=25004 if municipality =="CONCORDIA"
replace uniqueid=25005 if municipality =="COSALA"
replace uniqueid=25006 if municipality =="CULIACAN"
replace uniqueid=25010 if municipality =="EL FUERTE"
replace uniqueid=25008 if municipality =="ELOTA"
replace uniqueid=25009 if municipality =="ESCUINAPA"
replace uniqueid=25011 if municipality =="GUASAVE"
replace uniqueid=25012 if municipality =="MAZATLAN"
replace uniqueid=25013 if municipality =="MOCORITO"
replace uniqueid=25018 if municipality =="NAVOLATO"
replace uniqueid=25014 if municipality =="ROSARIO"
replace uniqueid=25015 if municipality =="SALVADOR ALVARADO"
replace uniqueid=25016 if municipality =="SAN IGNACIO"
replace uniqueid=25017 if municipality =="SINALOA"

egen valid = rowtotal(PAN_PRD_PT_PC PRI_PVEM_PANAL)

foreach var in PAN_PRD_PT_PC PRI_PVEM_PANAL listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PT_PC inv_mun_PRI_PVEM_PANAL, gen(PAN_PRD_PT_PC_r PRI_PVEM_PANAL_r)
drop inv_mun_*

gen winner = "PAN_PRD_PT_PC" if PAN_PRD_PT_PC_r==1  
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
drop *_r

gen year = 2010
gen month ="July"

sort section

save Sinaloa_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

use Ayu_Seccion_2013.dta, clear

gen turnout =  total/listanominal

drop NoRegistrados Nulos

gen   uniqueid= 0
replace uniqueid=25001 if municipality =="AHOME"
replace uniqueid=25002 if municipality =="ANGOSTURA"
replace uniqueid=25003 if municipality =="BADIRAGUATO"
replace uniqueid=25007 if municipality =="CHOIX"
replace uniqueid=25004 if municipality =="CONCORDIA"
replace uniqueid=25005 if municipality =="COSALA"
replace uniqueid=25006 if municipality =="CULIACAN"
replace uniqueid=25010 if municipality =="EL FUERTE"
replace uniqueid=25008 if municipality =="ELOTA"
replace uniqueid=25009 if municipality =="ESCUINAPA"
replace uniqueid=25011 if municipality =="GUASAVE"
replace uniqueid=25012 if municipality =="MAZATLAN"
replace uniqueid=25013 if municipality =="MOCORITO"
replace uniqueid=25018 if municipality =="NAVOLATO"
replace uniqueid=25014 if municipality =="ROSARIO"
replace uniqueid=25015 if municipality =="SALVADOR ALVARADO"
replace uniqueid=25016 if municipality =="SAN IGNACIO"
replace uniqueid=25017 if municipality =="SINALOA"

foreach var in PAN_PRD_PT PRI_PVEM_PANAL PAS PC PRI_PVEM_PANAL_PAS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD_PT inv_mun_PRI_PVEM_PANAL inv_mun_PAS inv_mun_PC inv_mun_PRI_PVEM_PANAL_PAS, gen(PAN_PRD_PT_r PRI_PVEM_PANAL_r PAS_r PC_r PRI_PVEM_PANAL_PAS_r)
drop inv_mun_*

gen winner = "PAN_PRD_PT" if PAN_PRD_PT_r==1  
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRI_PVEM_PANAL_PAS" if PRI_PVEM_PANAL_PAS_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2013
gen month ="July"

sort section

save Sinaloa_Section_2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Sinaloa_Section_2001.dta
append using Sinaloa_Section_2004.dta
append using Sinaloa_Section_2007.dta
append using Sinaloa_Section_2010.dta
append using Sinaloa_Section_2013.dta

erase Sinaloa_Section_2001.dta
erase Sinaloa_Section_2004.dta
erase Sinaloa_Section_2007.dta
erase Sinaloa_Section_2010.dta
erase Sinaloa_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

tab winner winner_counter

saveold Sinaloa_ALL.dta, replace version(12)

capture cd "C:\Users\jmarshall\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct"

saveold Sinaloa_ALL.dta, replace version(12)
