////////////////////////////////////////////
////////NAYARIT PRECINCT PANEL UPDATE///////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Nayarit"

import excel using "Ayuntamientos_2014.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
  clear
  local sheetname=r(worksheet_`sheet')
  import excel "Ayuntamientos_2014.xlsx", sheet("`sheetname'") clear firstrow allstring
  save "`sheetname'.dta", replace
}
*Run last chunk twice to copy filenames and replace with crtl+H
clear
append using "Table 2.dta"
append using "Table 3.dta"
append using "Table 4.dta"
append using "Table 5.dta"
append using "Table 6.dta"
append using "Table 7.dta"
append using "Table 8.dta"
append using "Table 9.dta"
append using "Table 10.dta"
append using "Table 11.dta"
append using "Table 12.dta"
append using "Table 13.dta"
append using "Table 14.dta"
append using "Table 15.dta"
append using "Table 16.dta"
append using "Table 17.dta"
append using "Table 18.dta"
append using "Table 20.dta"
append using "Table 21.dta"
append using "Table 19.dta"

keep if municipality!=""
destring *, replace

gen uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLÁN"
replace uniqueid=18003 if municipality =="AMATLÁN DE CAÑAS"
replace uniqueid=18020 if municipality =="BAHÍA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="DEL NAYAR"
replace uniqueid=18005 if municipality =="HUAJICORI"
replace uniqueid=18006 if municipality =="IXTLÁN DEL RÍO"
replace uniqueid=18007 if municipality =="JALA"
replace uniqueid=18019 if municipality =="LA YESCA"
replace uniqueid=18010 if municipality =="ROSAMORADA"
replace uniqueid=18011 if municipality =="RUÍZ"
replace uniqueid=18012 if municipality =="SAN BLAS"
replace uniqueid=18013 if municipality =="SAN PEDRO LAGUNILLAS"
replace uniqueid=18014 if municipality =="SANTA MARIA DEL ORO"
replace uniqueid=18015 if municipality =="SANTIAGO IXCUINTLA"
replace uniqueid=18016 if municipality =="TECUALA"
replace uniqueid=18017 if municipality =="TEPIC"
replace uniqueid=18018 if municipality =="TUXPAN"
replace uniqueid=18008 if municipality =="XALISCO"

rename (Sección CandidatoNoReg VotosNulos) (section no_reg nulo)

collapse (sum) PRI_PVEM_PANAL-CI_1, by (municipality uniqueid section)

order municipality section uniqueid PAN PRI_PVEM_PANAL PRD PT MC PRS CI_1 no_reg nulo

egen valid= rowtotal(PAN PRI_PVEM_PANAL PRD PT MC PRS CI_1)

gen total= valid + no_reg + nulo					

foreach var in PAN PRI_PVEM_PANAL PRD PT MC PRS CI_1 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI_PVEM_PANAL i_PRD i_PT i_MC i_PRS i_CI_1, gen(PAN_r PRI_PVEM_PANAL_r PRD_r PT_r MC_r PRS_r CI_1_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PANAL PRD PT MC PRS CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

drop no_reg nulo

replace winner="Independent" if winner=="CI_1"
replace second="Independent" if second=="CI_1"
replace third="Independent" if third=="CI_1"

gen year=2014
gen month="July"
gen STATE="NAYARIT"

order STATE municipality section uniqueid *

preserve
use "..\Listas Nominales\LN 2012-2019\2014\LN2014.dta", clear
*Lista nominal at the last date before the election (May 31st 2014) 
keep entidad municipio seccion lista file year month
keep if entidad==18 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN14_NAY.dta, replace
restore

merge 1:1 section using LN14_NAY.dta
drop if _merge==2
drop _merge
erase LN14_NAY.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

gen mun_turnout =  mun_total/mun_listanominal
gen turnout=total/listanominal

save Nayarit_Section_2014.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent

save incumbents17.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Nayarit_Section_2014.dta
collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Nayarit_2014.dta", replace

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

//////////////////////////////////
////////////2017//////////////////
//////////////////////////////////

import excel using "Ayuntamientos_2017.xlsx", first sheet("AYUNTAMIENTOS") clear

gen uniqueid= 0
replace uniqueid=18001 if municipality =="ACAPONETA"
replace uniqueid=18002 if municipality =="AHUACATLAN"
replace uniqueid=18003 if municipality =="AMATLAN DE CAÑAS"
replace uniqueid=18020 if municipality =="BAHIA DE BANDERAS"
replace uniqueid=18004 if municipality =="COMPOSTELA"
replace uniqueid=18009 if municipality =="DEL NAYAR"
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

drop C_PRI_PVEM_PANAL CP_PRI_PVEM CP_PRI_PANAL CP_PVEM_PANAL

egen PAN_PRD_PT_PRS = rowtotal(C_PAN_PRD_PT_PRS-CP_PT_PRS)
replace PAN_PRD_PT_PRS = PAN + PRD + PT + PRS
drop C_PAN_PRD_PT_PRS-CP_PT_PRS PAN PRD PT PRS
order PAN_PRD_PT_PRS, b(PRI)

rename TOTAL total

drop CAND_IND_4 CAND_IND_5
rename CAND_IND_* CI_*

collapse (sum) PAN_PRD_PT_PRS-CI_3 total listanominal, by (STATE municipality uniqueid section)

egen valid= rowtotal(PAN_PRD_PT_PRS PRI PVEM MC PANAL MORENA PES CI_1 CI_2 CI_3)

foreach var in PAN_PRD_PT_PRS PRI PVEM MC PANAL MORENA PES CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal

rowranks i_PAN_PRD_PT_PRS i_PRI i_PVEM i_MC i_PANAL i_MORENA i_PES i_CI_1 i_CI_2 i_CI_3, gen(PAN_PRD_PT_PRS_r PRI_r PVEM_r MC_r PANAL_r MORENA_r PES_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN_PRD_PT_PRS PRI PVEM MC PANAL MORENA PES CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"   | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

gen year=2017
gen month="June"

order STATE municipality section uniqueid *

save Nayarit_Section_2017.dta, replace

merge m:1 uniqueid using incumbents17.dta
drop _merge
erase incumbents17.dta

gen incumbent_vote=.
replace incumbent_vote=CI_1 if uniqueid==18012
replace incumbent_vote=PAN_PRD_PT_PRS if incumbent=="PAN"
replace incumbent_vote=PRI if incumbent=="PRI_PVEM_PANAL"

save Nayarit_Section_2017.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PRI-total incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Nayarit_2017.dta", replace

******************************************************************************************************************
******************************************************************************************************************

append using Nayarit_Section_2014.dta
append using Nayarit_Section_2017.dta

g PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
g winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Nayarit_Section_14_17.dta, replace

use "..\..\Precinct\Nayarit_ALL.dta", clear
append using Nayarit_Section_14_17.dta

erase Nayarit_Section_2014.dta
erase Nayarit_Section_2017.dta
erase Nayarit_Section_14_17.dta

save Nayarit_ALL_SALVADOR.dta, replace

******************************************************************************************************************
******************************************************************************************************************

erase "Table 2.dta"
erase "Table 3.dta"
erase "Table 4.dta"
erase "Table 5.dta"
erase "Table 6.dta"
erase "Table 7.dta"
erase "Table 8.dta"
erase "Table 9.dta"
erase "Table 10.dta"
erase "Table 11.dta"
erase "Table 12.dta"
erase "Table 13.dta"
erase "Table 14.dta"
erase "Table 15.dta"
erase "Table 16.dta"
erase "Table 17.dta"
erase "Table 18.dta"
erase "Table 20.dta"
erase "Table 21.dta"
erase "Table 19.dta"
