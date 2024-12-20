
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Audits\Election Data\Tabasco - 1997, 2000, 2003,2006, 2009, 2012"
capture cd "C:\Users\johnlouismarshall\Dropbox\Audits\Election Data\Tabasco - 1997, 2000, 2003,2006, 2009, 2012"


**************************************************************************
**************************************************************************
**************************************************************************



insheet using Ayu_Seccion_1997_No_LN.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PartidoCardenista
rename pps  PPS
* Partido Demócrata Mexicano
rename pdm  PDM

* gen turnout =  total/listanominal

drop  nulos 

gen   uniqueid= 0

replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MÉNDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAÍSO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

egen valid = rowtotal(PAN PRI PRD PartidoCardenista PT PVEM PPS PDM)

foreach var in PAN PRI PRD PartidoCardenista PT PVEM PPS PDM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartidoCardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PDM, gen(PAN_r PRI_r PRD_r PartidoCardenista_r PT_r PVEM_r PPS_r PDM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PartidoCardenista" if PartidoCardenista_r ==1 
replace winner = "PPS" if PPS_r ==1 
replace winner = "PDM" if PDM_r ==1 
drop *_r

gen year = 1997
gen month ="November"

save Tabasco_Section_1997.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2000_No_LN.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)
 
rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename cdppn   PC
* Partido de Centro Democrático
rename pcd  PCD
* Partido Democracia Social
rename dsppn  PDS
rename pas  PAS
rename psn  PSN
rename parm  PARM

* gen turnout =  total/listanominal

drop nulos noregistrados 

gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MENDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAISO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS)

foreach var in PAN PRI PRD PT PVEM PC PCD PSN PARM PAS PDS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

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
gen month ="October"

save Tabasco_Section_2000.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


insheet using Ayu_Seccion_2003.csv, clear

rename municipio  municipality
rename seccin section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan - total , replace

collapse (sum)  listanominal pan - total , by (municipality section)
 
rename pan  PAN
rename pripvem  PRI_PVEM
rename prd  PRD
rename pt   PT
rename pc   PC 
rename pas  PAS
rename pmp  PMP
rename fc  PFC

gen turnout =  total/listanominal

drop nulos noregistrados 

gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCÁN"
replace uniqueid=27002 if municipality =="CÁRDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACÁN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="Jalpa de Mendez"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAÍSO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

egen valid = rowtotal(PAN PRI_PVEM PRD PT PC PAS PMP PFC)

foreach var in PAN PRI_PVEM PRD PT PC PAS PMP PFC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PAS inv_mun_PMP inv_mun_PFC, gen(PAN_r PRI_PVEM_r PRD_r PT_r PC_r PAS_r PMP_r PFC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PAS" if PAS_r ==1 
replace winner = "PMP" if PMP_r ==1 
replace winner = "PFC" if PFC_r ==1 
drop *_r

gen year = 2003
gen month ="October"

sort section

save Tabasco_Section_2003.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "estadistica_electoral_2006.xls", sheet("Casillas Regidores") cellrange(A4:L2429) firstrow clear

rename MUNICIPIO municipality
replace municipality=upper(subinstr(municipality, "Á", "A",.))
replace municipality=upper(subinstr(municipality, "É", "E",.)) 
replace municipality=upper(subinstr(municipality, "Í", "I",.)) 
replace municipality=upper(subinstr(municipality, "Ó", "O",.)) 
replace municipality=upper(subinstr(municipality, "Ú", "U",.)) 
replace municipality=upper(subinstr(municipality, "á", "a",.))
replace municipality=upper(subinstr(municipality, "é", "e",.)) 
replace municipality=upper(subinstr(municipality, "í", "i",.)) 
replace municipality=upper(subinstr(municipality, "ó", "o",.)) 
replace municipality=upper(subinstr(municipality, "ú", "u",.)) 
replace municipality=upper(subinstr(municipality, "Ñ", "N",.)) 
replace municipality=upper(subinstr(municipality, "ñ", "n",.)) 
replace municipality=upper(subinstr(municipality, "ü", "u",.)) 
replace municipality = upper(municipality)
rename SECCIN section

rename CPBT PRD_PT
rename NUEVAALIANZA PANAL
rename ALTERNATIVA PAS
rename CANDIDATOSNOREGISTRADOS noregistrados 
rename VOTOSNULOS nulos 
rename VOTACINTOTAL total

drop if municipality=="" & section==.
drop if total==. | total==0 

collapse (sum) PAN - total , by (municipality section)
 
*gen turnout =  total/listanominal

drop nulos noregistrados 

gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MENDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAISO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"

egen valid = rowtotal(PAN PRI PRD_PT PVEM PANAL PAS)

foreach var in PAN PRI PRD_PT PVEM PANAL PAS total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT inv_mun_PVEM inv_mun_PANAL inv_mun_PAS, gen(PAN_r PRI_r PRD_PT_r PVEM_r PANAL_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PAS" if PAS_r ==1 
drop *_r


gen year = 2006
gen month ="October"

sort section

save Tabasco_Section_2006.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0

destring listanominal  pan - total , replace

collapse (sum)  pan  - total (first) listanominal, by (municipality section)

replace pripanal = pri + panal + pripanal
drop pri panal

rename pan PAN
rename pripanal PRI_PANAL
rename prd PRD
rename pt PT
rename pvem PVEM
rename convergencia PC

gen turnout =  total/listanominal

drop nulos noregistrados 

gen   uniqueid= 0
replace uniqueid=27001 if municipality =="BALANCAN"
replace uniqueid=27002 if municipality =="CARDENAS"
replace uniqueid=27003 if municipality =="CENTLA"
replace uniqueid=27004 if municipality =="CENTRO"
replace uniqueid=27005 if municipality =="COMALCALCO"
replace uniqueid=27006 if municipality =="CUNDUACAN"
replace uniqueid=27007 if municipality =="EMILIANO ZAPATA"
replace uniqueid=27008 if municipality =="HUIMANGUILLO"
replace uniqueid=27009 if municipality =="JALAPA"
replace uniqueid=27010 if municipality =="JALPA DE MENDEZ"
replace uniqueid=27011 if municipality =="JONUTA"
replace uniqueid=27012 if municipality =="MACUSPANA"
replace uniqueid=27013 if municipality =="NACAJUCA"
replace uniqueid=27014 if municipality =="PARAISO"
replace uniqueid=27015 if municipality =="TACOTALPA"
replace uniqueid=27016 if municipality =="TEAPA"
replace uniqueid=27017 if municipality =="TENOSIQUE"


egen valid = rowtotal(PAN PRI_PANAL PRD PT PVEM PC )

foreach var in PAN PRI_PANAL PRD PT PVEM PC  listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PANAL inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC, gen(PAN_r PRI_PANAL_r PRD_r PT_r PVEM_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2009
gen month ="October"

sort section

save Tabasco_Section_2009.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "Ayu_Seccion_2012.xlsx", sheet("Sheet1") firstrow clear

drop if MUNICIPIO==.
rename SECCION section
drop CASILLA

gen PRD_PT_PC = CC_PRD_PT_PC + PRD + PT + PC
gen PRI_PVEM_PANAL =  CC_PRI_PVEM_PANAL + PRI + PVEM + PANAL
drop CC_PRD_PT_PC PRD PT PC CC_PRI_PVEM_PANAL PRI PVEM PANAL

collapse (sum) PAN- PRI_PVEM_PANAL, by (MUNICIPIO section)

egen total = rowtotal(PRD_PT_PC PRI_PVEM_PANAL NOREGISTRADOS NULOS)
drop NOREGISTRADOS NULOS

* gen turnout = total / listanominal

gen   uniqueid= 0
gen municipality=""
replace uniqueid=27001 if MUNICIPIO==1
replace municipality ="BALANCÁN" if MUNICIPIO==1
replace uniqueid=27002  if MUNICIPIO==2
replace municipality ="CÁRDENAS"  if MUNICIPIO==2
replace uniqueid=27003 if MUNICIPIO==3
replace municipality ="CENTLA" if MUNICIPIO==3
replace uniqueid=27004 if MUNICIPIO==4 
replace municipality ="CENTRO" if MUNICIPIO==4
replace uniqueid=27005 if MUNICIPIO==5
replace municipality ="COMALCALCO" if MUNICIPIO==5
replace uniqueid=27006 if MUNICIPIO==6
replace municipality ="CUNDUACÁN" if MUNICIPIO==7
replace uniqueid=27007 if MUNICIPIO==7
replace municipality ="EMILIANO ZAPATA" if MUNICIPIO==7
replace uniqueid=27008 if MUNICIPIO==8 
replace municipality ="HUIMANGUILLO" if MUNICIPIO==8
replace uniqueid=27009 if MUNICIPIO==9
replace municipality ="JALAPA" if MUNICIPIO==9
replace uniqueid=27010 if MUNICIPIO==10 
replace municipality ="Jalpa de Mendez" if MUNICIPIO==10
replace uniqueid=27011 if MUNICIPIO==11 
replace municipality ="JONUTA" if MUNICIPIO==11
replace uniqueid=27012 if MUNICIPIO==12
replace municipality ="MACUSPANA" if MUNICIPIO==12
replace uniqueid=27013 if MUNICIPIO==13
replace municipality ="NACAJUCA" if MUNICIPIO==13
replace uniqueid=27014 if MUNICIPIO==14
replace municipality ="PARAÍSO" if MUNICIPIO==14
replace uniqueid=27015 if MUNICIPIO==15
replace municipality ="TACOTALPA" if MUNICIPIO==15
replace uniqueid=27016 if MUNICIPIO==16
replace municipality ="TEAPA" if MUNICIPIO==16
replace uniqueid=27017 if MUNICIPIO==17
replace municipality ="TENOSIQUE" if MUNICIPIO==17
drop MUNICIPIO

egen valid = rowtotal(PAN PRD_PT_PC PRI_PVEM_PANAL )

foreach var in PAN PRD_PT_PC PRI_PVEM_PANAL total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD_PT_PC inv_mun_PRI_PVEM_PANAL, gen(PAN_r PRD_PT_PC_r PRI_PVEM_PANAL_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRD_PT_PC" if PRD_PT_PC_r ==1 
drop *_r

gen year = 2012
gen month ="July"

sort section

save Tabasco_Section_2012.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use Tabasco_Section_1997.dta
append using Tabasco_Section_2000.dta
append using Tabasco_Section_2003.dta
append using Tabasco_Section_2006.dta
append using Tabasco_Section_2009.dta
append using Tabasco_Section_2012.dta

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

save Tabasco_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Audits\Election Data"
capture cd "C:\Users\johnlouismarshall\Dropbox\Audits\Election Data"


save Tabasco_ALL.dta, replace
