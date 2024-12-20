////////////////////////////////////////////
////////JALISCO PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Jalisco"

import excel "ResultadosPorCasilla2015.xlsx", sheet("RESULTADOS") clear firstrow allstring

rename Municipio MUNICIPIO
rename Sección SECCION
replace Nulos="" if Nulos=="NULL"
replace NoRegistrados="" if NoRegistrados=="NULL"

keep if Elección=="Municipes"

destring *, replace

gen CI_1=0
replace CI_1=JESÚSOSWALDOSILVAMAGAÑA if uniqueid==14108
replace CI_1=JOSEZEPEDACONTRERAS if uniqueid==14112
replace CI_1=JOSEFRANCISCOSANCHEZPEREZ if uniqueid==14067
replace CI_1=GUILLERMOSIENFUEGOSPEREZ if uniqueid==14039

*Pending: check if boletas=listanominal
drop JOSEPEDROKUMAMOTOAGUILAR JESÚSOSWALDOSILVAMAGAÑA JOSEZEPEDACONTRERAS JOSEFRANCISCOSANCHEZPEREZ GUILLERMOSIENFUEGOSPEREZ Boletas Votos*

replace PRI_PVEM=PRI_PVEM+PRI+PVEM if coalPRIPVEM==1
replace PRI=0 if coalPRIPVEM==1
replace PVEM=0 if coalPRIPVEM==1

replace PAN_PRD=PAN_PRD + PAN + PRD if coalPANPRD==1
replace PAN=0 if coalPANPRD==1
replace PRD=0 if coalPANPRD==1

rename Nulos nulo
rename NoRegistrados no_reg
rename NA PANAL

**********CHECK
collapse(sum) PAN-CI_1 (first) MUNICIPIO uniqueid , by ( SECCION ) 

egen valid= rowtotal(PAN PRI PRD PT PVEM MC PANAL MORENA PES PH PAN_PRD PRI_PVEM CI_1)

gen total=valid+ nulo + no_reg

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES PH PAN_PRD PRI_PVEM CI_1 total valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_MORENA i_PES i_PH i_PAN_PRD i_PRI_PVEM /// 
         i_CI_1, gen(PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r MORENA_r PES_r PH_r PAN_PRD_r PRI_PVEM_r CI_1_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL MORENA PES PH PAN_PRD PRI_PVEM CI_1 {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3
}

drop *_r

drop no_reg 

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1" 
replace third="Independent" if third=="CI_1" 

gen year=2015
gen month="June"
gen STATE="JALISCO"

rename MUNICIPIO municipality
rename SECCION section

save Jalisco_Section_2015.dta, replace
collapse (first) winner, by (municipality uniqueid)
rename winner incumbent
save incumbents2018.dta, replace 

*sort section
*quietly by section:  gen dup = cond(_N==1,0,_n)
*tab dup

*Sections incorrectly labeled in data from OPLE. Checked at http://www.iepcjalisco.org.mx/geografia-electoral
clear
use Jalisco_Section_2015.dta
replace section=. if (uniqueid==14098 & section==995) | (uniqueid==14098 & section==1548) | (uniqueid==14101 & section==2024) | ///
        (uniqueid==14101 & section==2025) | (uniqueid==14044 & section==2462) | (uniqueid==14039 & (section==2484  | section==2485 | section==2486 ) ) | ///
		(uniqueid==14039 & section==2484) | (uniqueid==14101 & section==2563) | (uniqueid==14098 & section==2720)  | (uniqueid==14098 & section==2721) | ///
        (uniqueid==14070 & section==2729) | (uniqueid==14098 & section==3206)
save Jalisco_Section_2015.dta, replace


clear
use "..\Listas Nominales\LN 2012-2019\2015\LN2015.dta"
*Lista nominal at the last date before the election (June 30 2015) 
keep entidad municipio seccion lista file year month
keep if entidad==14 & month==6
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN15_JAL.dta, replace

clear
use Jalisco_Section_2015.dta
drop if section==.
merge 1:1 section using LN15_JAL.dta
drop if _merge==2
drop _merge
erase LN15_JAL.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

gen mun_turnout =  mun_total/mun_listanominal
gen turnout=total/listanominal

order STATE municipality uniqueid section year month *
sort uniqueid section

save Jalisco_Section_2015.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Jalisco_2015.dta", replace

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

import excel "Ayuntamientos_2018.xlsx", sheet("Ayuntamientos") clear firstrow allstring
drop if MUNICIPIO=="MUNICIPIO"
drop CASILLA
rename poll_station CASILLA
gen section=substr(CASILLA,1,4)
destring *, replace

g PT_MORENA_PES = .
replace PT_MORENA_PES = PESPTMORENA + PTMORENA + PTPES + MORENAPES + PT + MORENA + PES if coalpbt==1
replace PT=. if coalpbt==1
replace MORENA=. if coalpbt==1
replace PES=. if coalpbt==1
drop PESPTMORENA PTMORENA PTPES MORENAPES  

g PAN_PRD_MC = .
replace PAN_PRD_MC = PANPRDMC + PANPRD + PANMC + PRDMC + PAN + PRD + MC if coaljf==1
replace PAN=. if coaljf==1
replace PRD=. if coaljf==1
replace MC=. if coaljf==1
drop PANPRDMC PANPRD PANMC PRDMC  

rename NOREGISTRADOS no_reg
rename VOTOSNULOS nulo
rename MUNICIPIO municipality

collapse (sum) PAN-CI_5 PT_MORENA_PES PAN_PRD_MC (first) coaljf coalpbt, by (municipality uniqueid section)

rename NA PANAL

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL MORENA PES PT_MORENA_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 )

gen total=valid+ nulo + no_reg

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PT_MORENA_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 total valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_MORENA i_PES i_PT_MORENA_PES i_PAN_PRD_MC /// 
         i_CI_1 i_CI_2 i_CI_3 i_CI_4 i_CI_5 , gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r MORENA_r PES_r PT_MORENA_PES_r PAN_PRD_MC_r ///
		 CI_1_r CI_2_r CI_3_r CI_4_r CI_5_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL MORENA PES PT_MORENA_PES PAN_PRD_MC CI_1 CI_2 CI_3 CI_4 CI_5 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

drop no_reg 

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1" 
replace third="Independent" if third=="CI_1" 

gen year=2018
gen month="July"
gen STATE="JALISCO"

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta

gen incumbent_vote = .

replace incumbent_vote=PAN if incumbent=="PAN" & coaljf==0
replace incumbent_vote=PRD if incumbent=="PRD" & coaljf==0
replace incumbent_vote=MC if incumbent=="MC" & coaljf==0
replace incumbent_vote=PAN_PRD_MC if ( incumbent=="PAN" | incumbent=="PRD" | incumbent=="MC" | incumbent=="PAN_PRD") & coaljf==1
* No MC standing
replace incumbent_vote = . if uniqueid==14022

replace incumbent_vote=PT if incumbent=="PT" & coalpbt==0
replace incumbent_vote=PES if  incumbent=="PES" & coalpbt==0
replace incumbent_vote=PT_MORENA_PES if (incumbent=="PT" | incumbent=="PES") & coalpbt==1

replace incumbent_vote=PANAL if incumbent=="PANAL" 

replace incumbent_vote=PRI if incumbent=="PRI" 

replace incumbent_vote=PVEM if incumbent=="PVEM" 

*Take max as incumbents listed as PRI-PVEM without party affiliation
egen maxpripvem=rowmax(PRI PVEM)
replace incumbent_vote=maxpripvem if incumbent=="PRI_PVEM"

*Take max as incumbents listed as PAN-PRD without party affiliation
egen maxpanprd=rowmax(PAN PRD)
replace incumbent_vote=maxpanprd if incumbent=="PAN_PRD" & coaljf==0

drop max*
drop coal*

preserve
use "..\Listas Nominales\ListadoNominalPREP2018.dta", clear
keep if STATE=="JALISCO"
save JAL_LN18.dta, replace
restore

merge 1:1 STATE section using JAL_LN18.dta
drop _merge
erase JAL_LN18.dta 

rename ListadoNominalINE listanominal

gen turnout=total/listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 
gen mun_turnout =  mun_total/mun_listanominal

save "Jalisco_Section_2018.dta", replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-total listanominal incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Jalisco_2018.dta", replace

*************************************************************************************************************************************************
*************************************************************************************************************************************************
*************************************************************************************************************************************************

clear
append using Jalisco_Section_2018.dta
append using Jalisco_Section_2015.dta

erase Jalisco_Section_2015.dta
erase Jalisco_Section_2018.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Jalisco_Section_15_18.dta, replace

use "..\..\Precinct\Jalisco_ALL.dta", clear
append using Jalisco_Section_15_18.dta
save Jalisco_ALL_SALVADOR.dta, replace

erase Jalisco_Section_15_18.dta
