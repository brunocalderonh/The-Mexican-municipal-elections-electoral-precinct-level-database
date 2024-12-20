////////////////////////////////////////////
////////VERACRUZ PRECINCT PANEL UPDATE//////
////////JUL 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Veracruz"

import excel using "Ayuntamientos_2016.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2016.xlsx", sheet("`sheetname'") clear firstrow allstring
	replace municipality=municipality[1]
	drop if Seccion=="TOTAL" | Seccion==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "Resultados.dta"
append using "Resultados (2).dta"
append using "Resultados (3).dta"
append using "Resultados (4).dta"
append using "Resultados (5).dta"
append using "Resultados (6).dta"
append using "Resultados (7).dta"
append using "Resultados (8).dta"
append using "Resultados (10).dta"
append using "Resultados (11).dta"
append using "Resultados (12).dta"
append using "Resultados (13).dta"
append using "Resultados (14).dta"
append using "Resultados (15).dta"
append using "Resultados (16).dta"
append using "Resultados (17).dta"
append using "Resultados (18).dta"
append using "Resultados (19).dta"
append using "Resultados (20).dta"
append using "Resultados (21).dta"
append using "Resultados (22).dta"
append using "Resultados (23).dta"
append using "Resultados (24).dta"
append using "Resultados (25).dta"
append using "Resultados (26).dta"
append using "Resultados (27).dta"
append using "Resultados (28).dta"
append using "Resultados (29).dta"
append using "Resultados (30).dta"
append using "Resultados (31).dta"
append using "Resultados (32).dta"
append using "Resultados (33).dta"
append using "Resultados (34).dta"
append using "Resultados (35).dta"
append using "Resultados (36).dta"
append using "Resultados (37).dta"
append using "Resultados (38).dta"
append using "Resultados (39).dta"
append using "Resultados (40).dta"
append using "Resultados (41).dta"
append using "Resultados (42).dta"
append using "Resultados (43).dta"
append using "Resultados (44).dta"
append using "Resultados (45).dta"
append using "Resultados (46).dta"
append using "Resultados (47).dta"
append using "Resultados (48).dta"
append using "Resultados (49).dta"
append using "Resultados (50).dta"
append using "Resultados (51).dta"
append using "Resultados (52).dta"
append using "Resultados (53).dta"
append using "Resultados (54).dta"
append using "Resultados (55).dta"
append using "Resultados (56).dta"
append using "Resultados (57).dta"
append using "Resultados (58).dta"
append using "Resultados (59).dta"
append using "Resultados (60).dta"
append using "Resultados (61).dta"
append using "Resultados (62).dta"
append using "Resultados (63).dta"
append using "Resultados (64).dta"
append using "Resultados (65).dta"
append using "Resultados (66).dta"
append using "Resultados (67).dta"
append using "Resultados (68).dta"
append using "Resultados (69).dta"
append using "Resultados (70).dta"
append using "Resultados (71).dta"
append using "Resultados (72).dta"
append using "Resultados (73).dta"
append using "Resultados (74).dta"
append using "Resultados (75).dta"
append using "Resultados (76).dta"
append using "Resultados (77).dta"
append using "Resultados (78).dta"
append using "Resultados (79).dta"
append using "Resultados (80).dta"
append using "Resultados (81).dta"
append using "Resultados (82).dta"
append using "Resultados (83).dta"
append using "Resultados (84).dta"
append using "Resultados (85).dta"
append using "Resultados (86).dta"
append using "Resultados (87).dta"
append using "Resultados (88).dta"
append using "Resultados (89).dta"
append using "Resultados (90).dta"
append using "Resultados (91).dta"
append using "Resultados (92).dta"
append using "Resultados (93).dta"
append using "Resultados (94).dta"
append using "Resultados (95).dta"
append using "Resultados (96).dta"
append using "Resultados (97).dta"
append using "Resultados (98).dta"
append using "Resultados (99).dta"
append using "Resultados (100).dta"
append using "Resultados (101).dta"
append using "Resultados (102).dta"
append using "Resultados (103).dta"
append using "Resultados (104).dta"
append using "Resultados (105).dta"
append using "Resultados (106).dta"
append using "Resultados (107).dta"
append using "Resultados (108).dta"
append using "Resultados (109).dta"
append using "Resultados (110).dta"
append using "Resultados (111).dta"
append using "Resultados (112).dta"
append using "Resultados (113).dta"
append using "Resultados (114).dta"
append using "Resultados (115).dta"
append using "Resultados (116).dta"
append using "Resultados (117).dta"
append using "Resultados (118).dta"
append using "Resultados (119).dta"
append using "Resultados (120).dta"
append using "Resultados (121).dta"
append using "Resultados (122).dta"
append using "Resultados (123).dta"
append using "Resultados (124).dta"
append using "Resultados (125).dta"
append using "Resultados (126).dta"
append using "Resultados (127).dta"
append using "Resultados (128).dta"
append using "Resultados (129).dta"
append using "Resultados (130).dta"
append using "Resultados (131).dta"
append using "Resultados (132).dta"
append using "Resultados (133).dta"
append using "Resultados (134).dta"
append using "Resultados (135).dta"
append using "Resultados (136).dta"
append using "Resultados (137).dta"
append using "Resultados (138).dta"
append using "Resultados (139).dta"
append using "Resultados (140).dta"
append using "Resultados (141).dta"
append using "Resultados (142).dta"
append using "Resultados (143).dta"
append using "Resultados (144).dta"
append using "Resultados (145).dta"
append using "Resultados (146).dta"
append using "Resultados (147).dta"
append using "Resultados (148).dta"
append using "Resultados (149).dta"
append using "Resultados (150).dta"
append using "Resultados (151).dta"
append using "Resultados (152).dta"
append using "Resultados (153).dta"
append using "Resultados (154).dta"
append using "Resultados (155).dta"
append using "Resultados (156).dta"
append using "Resultados (157).dta"
append using "Resultados (158).dta"
append using "Resultados (159).dta"
append using "Resultados (160).dta"
append using "Resultados (161).dta"
append using "Resultados (162).dta"
append using "Resultados (164).dta"
append using "Resultados (165).dta"
append using "Resultados (166).dta"
append using "Resultados (167).dta"
append using "Resultados (168).dta"
append using "Resultados (169).dta"
append using "Resultados (171).dta"
append using "Resultados (172).dta"
append using "Resultados (173).dta"
append using "Resultados (174).dta"
append using "Resultados (175).dta"
append using "Resultados (176).dta"
append using "Resultados (177).dta"
append using "Resultados (178).dta"

gen municipio=subinstr(municipality,"Reporte de Casillas del municipio de ","",1)

save Veracruz_Section_2017.dta, replace

****************************************************************************************************************************************

clear
cd "AyuntamientosB"

import excel using "Ayuntamientos_2016b.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2016b.xlsx", sheet("`sheetname'") clear firstrow allstring
	replace municipality=municipality[1]
	drop if Seccion=="TOTAL" | Seccion==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "Resultados.dta"
append using "Resultados (2).dta"
append using "Resultados (3).dta"
append using "Resultados (4).dta"
append using "Resultados (5).dta"
append using "Resultados (6).dta"
append using "Resultados (7).dta"
append using "Resultados (8).dta"
append using "Resultados (9).dta"
append using "Resultados (10).dta"
append using "Resultados (11).dta"
append using "Resultados (12).dta"
append using "Resultados (13).dta"
append using "Resultados (14).dta"
append using "Resultados (15).dta"
append using "Resultados (16).dta"
append using "Resultados (17).dta"
append using "Resultados (18).dta"
append using "Resultados (19).dta"
append using "Resultados (20).dta"
append using "Resultados (21).dta"
append using "Resultados (22).dta"
append using "Resultados (23).dta"
append using "Resultados (24).dta"
append using "Resultados (25).dta"
append using "Resultados (26).dta"
append using "Resultados (27).dta"
append using "Resultados (28).dta"
append using "Resultados (29).dta"
append using "Resultados (30).dta"
append using "Resultados (31).dta"
append using "Resultados (32).dta"
append using "Resultados (33).dta"
append using "Resultados (34).dta"
append using "Resultados (35).dta"
append using "Resultados (36).dta"

gen municipio=subinstr(municipality,"Reporte de Casillas del municipio de ","",1)

drop if municipio=="Jaltipan"
drop if municipio=="Zontecomatlán"

save Veracruz_Section_2017b.dta, replace

**************************************************************************************************************************************

cd ".."

use Veracruz_Section_2017.dta, clear
append using "AyuntamientosB\Veracruz_Section_2016b.dta"

destring *, replace

drop O P
drop municipality
rename municipio municipality
rename Seccion section
order municipality section *

replace CoalicionPANPRD=CoalicionPANPRD + PAN + PRD if CoalicionPANPRD!=.
replace PAN=. if CoalicionPANPRD!=.
replace PRD=. if CoalicionPANPRD!=.

replace CoaliciónPANPRD=CoaliciónPANPRD+ PAN + PRD if CoaliciónPANPRD!=.
replace PAN=. if CoaliciónPANPRD!=.
replace PRD=. if CoaliciónPANPRD!=.

rename VERDE PVEM
replace CoalicionPRIVERDE=CoalicionPRIVERDE + PRI + PVEM if CoalicionPRIVERDE!=.
replace PRI=. if CoalicionPRIVERDE!=.
replace PVEM=. if CoalicionPRIVERDE!=.

replace CoalicionPriverde=CoalicionPriverde + PRI + PVEM if CoalicionPriverde!=.
replace PRI=. if CoalicionPriverde!=.
replace PVEM=. if CoalicionPriverde!=.

collapse (sum) PAN-MayrethMartínezPeñaloza, by (municipality section)

gen nulo=Q + VotosNulos
drop Q VotosNulos
gen PRI_PVEM=CoalicionPRIVERDE+CoalicionPriverde
drop CoalicionPRIVERDE CoalicionPriverde

gen PANAL=NuevaAlianza + NuevaALianza +  Nuevaalianza
drop NuevaAlianza NuevaALianza Nuevaalianza

gen MC= MovimientoCiudadano + MovimientoCuidadano
drop MovimientoCiudadano MovimientoCuidadano
replace PT= PT + Pt + pt
drop Pt pt

gen PAN_PRD=CoaliciónPANPRD + CoalicionPANPRD
drop CoaliciónPANPRD  CoalicionPANPRD
rename (EncuentroSocial CNR) (PES no_reg) 

egen CI_1=rowtotal(ChistianRomeroPerez-MayrethMartínezPeñaloza)
egen CI_2=rowtotal(EduardoCidJuarez MauricioIvanAguilleMarín VíctorManuelMurrietaPérez MartínGarcíaMartínez MiguelÁngelMartínezDionisio /// 
                   OscarOctacioGreerBecerra EmilioÁlvarezPimentel AntonioLunaAndrade RafaelVegaHernández MayrethMartínezPeñaloza)
egen CI_3=rowtotal(ChristopherCristianCházaro RubénMorenoArcher)
replace CI_1=CI_1-CI_2-CI_3
drop ChistianRomeroPerez-MayrethMartínezPeñaloza

replace MORENA=Morena+MORENA
drop Morena

egen valid=rowtotal(PAN PRI PRD PVEM PT PANAL MC MORENA PES PRI_PVEM PAN_PRD CI_1 CI_2 CI_3)

gen total=valid+nulo+no_reg
drop nulo no_reg

preserve
import excel using "uniqueids.xlsx", first clear
save uniqueids16.dta, replace
restore

merge m:1 municipality using uniqueids16.dta
drop _merge
erase uniqueids16.dta

drop municipality
rename MUN municipality

foreach var in PAN PRD PRI PVEM PT MORENA PES PRI_PVEM PANAL MC PAN_PRD CI_1 CI_2 CI_3 total valid {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_PANAL i_MC i_MORENA i_PES i_PRI_PVEM i_PAN_PRD i_CI_1 i_CI_2 i_CI_3, gen(PAN_r PRI_r PRD_r PVEM_r PT_r PANAL_r MC_r MORENA_r PES_r PRI_PVEM_r PAN_PRD_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRD PRI PVEM PT MORENA PES PRI_PVEM PANAL MC PAN_PRD CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" | winner=="CI_2"   | winner=="CI_3"  
replace second="Independent" if second=="CI_1"  | second=="CI_2" | second=="CI_3"
replace third="Independent" if third=="CI_1" | third=="CI_2"  | third=="CI_3" 

g year=2017
g month="June"
g STATE="VERACRUZ"

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC PES Independent {
	g `var'_winner =0
	replace `var'_winner =1 if strpos(winner, "`var'")>0 
	replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

drop if section==.

preserve
use "..\Listas Nominales\LN 2012-2019\2017\LN2017.dta", clear
*Lista nominal at the last date before the election (May 31st 2017) 
keep entidad municipio seccion lista file year month
keep if entidad==30 & month==5
gen uniqueid=(entidad*1000)+ municipio
keep if seccion!=0
sort uniqueid seccion
keep uniqueid seccion lista
rename seccion section
save LN17_VER.dta, replace
restore

merge 1:1 section using LN17_VER.dta
drop if _merge==2
drop _merge
erase LN17_VER.dta

rename lista listanominal
bys uniqueid: egen mun_listanominal= sum(listanominal) 

g mun_turnout = mun_total/mun_listanominal
g turnout = total/listanominal
				   
save Veracruz_Section_2017.dta, replace			   

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Veracruz_Section_2017.dta
collapse (sum) PAN-total listanominal (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Veracruz_2017.dta", replace

**************************************************************************
**************************************************************************
**************************************************************************

import excel "VERACRUZ_EXTRAORDINARIAMPAL_2018.xlsx", sheet("VERACRUZ_EXTRAORDINARIAMPAL_201") cellrange(A7:AM138) firstrow clear

rename SECCION section
rename MUNICIPIO municipality
rename LISTA_NOMINAL listanominal
rename TOTAL total

g PAN_PRD = PAN + PRD + PANPRD
g PRI_PVEM = PRI + PVEM + PRIPVEM
drop PAN PRD PANPRD PRI PVEM PRIPVEM

g MORENA_PT_PES = MORENA + PES + PT + PTMORENAPES + PTPES + PTMORENA + MORENAPES if municipality=="EMILIANO ZAPATA"
drop PT MORENA PTMORENAPES PTPES PTMORENA MORENAPES
replace PES = . if municipality=="EMILIANO ZAPATA"

collapse (sum) PAN_PRD PRI_PVEM MORENA_PT_PES PANAL PES listanominal total, by (municipality section)

g uniqueid = .
replace uniqueid=30007 if municipality=="CAMARON DE TEJEDA"
replace uniqueid=30065 if municipality=="EMILIANO ZAPATA"
replace uniqueid=30144 if municipality=="SAYULA DE ALEMAN"

replace municipality = municipality + " EXTRAORDINARIO"

g turnout = total/listanominal

egen valid = rowtotal(PAN_PRD PRI_PVEM MORENA_PT_PES PANAL PES)

foreach var in PAN_PRD PRI_PVEM MORENA_PT_PES PANAL PES listanominal total valid {
	bys uniqueid: egen mun_`var'= sum(`var') if `var'!=.
	g inv_mun_`var'= 1/mun_`var'
}

g mun_turnout = mun_total/mun_listanominal

rowranks inv_mun_PAN_PRD inv_mun_PRI_PVEM inv_mun_MORENA_PT_PES inv_mun_PANAL inv_mun_PES, gen(PAN_PRD_r PRI_PVEM_r MORENA_PT_PES_r PANAL_r PES_r)
drop inv_mun_*

gen winner = "PAN_PRD" if PAN_PRD_r==1  
replace winner = "PRI_PVEM" if PRI_PVEM_r==1 
replace winner = "MORENA_PT_PES" if MORENA_PT_PES_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PES" if PES_r ==1 
drop *_r

gen year = 2018
gen month ="March"

save Veracruz_Section_2018.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

use "..\..\Precinct\Veracruz_ALL.dta", clear
append using Veracruz_Section_2017.dta
append using Veracruz_Section_2018.dta

erase Veracruz_Section_2017.dta
erase Veracruz_Section_2018.dta

save Veracruz_ALL_SALVADOR.dta, replace

***************************************************

erase "Resultados.dta"
erase "Resultados (2).dta"
erase "Resultados (3).dta"
erase "Resultados (4).dta"
erase "Resultados (5).dta"
erase "Resultados (6).dta"
erase "Resultados (7).dta"
erase "Resultados (8).dta"
erase "Resultados (10).dta"
erase "Resultados (11).dta"
erase "Resultados (12).dta"
erase "Resultados (13).dta"
erase "Resultados (14).dta"
erase "Resultados (15).dta"
erase "Resultados (16).dta"
erase "Resultados (17).dta"
erase "Resultados (18).dta"
erase "Resultados (19).dta"
erase "Resultados (20).dta"
erase "Resultados (21).dta"
erase "Resultados (22).dta"
erase "Resultados (23).dta"
erase "Resultados (24).dta"
erase "Resultados (25).dta"
erase "Resultados (26).dta"
erase "Resultados (27).dta"
erase "Resultados (28).dta"
erase "Resultados (29).dta"
erase "Resultados (30).dta"
erase "Resultados (31).dta"
erase "Resultados (32).dta"
erase "Resultados (33).dta"
erase "Resultados (34).dta"
erase "Resultados (35).dta"
erase "Resultados (36).dta"
erase "Resultados (37).dta"
erase "Resultados (38).dta"
erase "Resultados (39).dta"
erase "Resultados (40).dta"
erase "Resultados (41).dta"
erase "Resultados (42).dta"
erase "Resultados (43).dta"
erase "Resultados (44).dta"
erase "Resultados (45).dta"
erase "Resultados (46).dta"
erase "Resultados (47).dta"
erase "Resultados (48).dta"
erase "Resultados (49).dta"
erase "Resultados (50).dta"
erase "Resultados (51).dta"
erase "Resultados (52).dta"
erase "Resultados (53).dta"
erase "Resultados (54).dta"
erase "Resultados (55).dta"
erase "Resultados (56).dta"
erase "Resultados (57).dta"
erase "Resultados (58).dta"
erase "Resultados (59).dta"
erase "Resultados (60).dta"
erase "Resultados (61).dta"
erase "Resultados (62).dta"
erase "Resultados (63).dta"
erase "Resultados (64).dta"
erase "Resultados (65).dta"
erase "Resultados (66).dta"
erase "Resultados (67).dta"
erase "Resultados (68).dta"
erase "Resultados (69).dta"
erase "Resultados (70).dta"
erase "Resultados (71).dta"
erase "Resultados (72).dta"
erase "Resultados (73).dta"
erase "Resultados (74).dta"
erase "Resultados (75).dta"
erase "Resultados (76).dta"
erase "Resultados (77).dta"
erase "Resultados (78).dta"
erase "Resultados (79).dta"
erase "Resultados (80).dta"
erase "Resultados (81).dta"
erase "Resultados (82).dta"
erase "Resultados (83).dta"
erase "Resultados (84).dta"
erase "Resultados (85).dta"
erase "Resultados (86).dta"
erase "Resultados (87).dta"
erase "Resultados (88).dta"
erase "Resultados (89).dta"
erase "Resultados (90).dta"
erase "Resultados (91).dta"
erase "Resultados (92).dta"
erase "Resultados (93).dta"
erase "Resultados (94).dta"
erase "Resultados (95).dta"
erase "Resultados (96).dta"
erase "Resultados (97).dta"
erase "Resultados (98).dta"
erase "Resultados (99).dta"
erase "Resultados (100).dta"
erase "Resultados (101).dta"
erase "Resultados (102).dta"
erase "Resultados (103).dta"
erase "Resultados (104).dta"
erase "Resultados (105).dta"
erase "Resultados (106).dta"
erase "Resultados (107).dta"
erase "Resultados (108).dta"
erase "Resultados (109).dta"
erase "Resultados (110).dta"
erase "Resultados (111).dta"
erase "Resultados (112).dta"
erase "Resultados (113).dta"
erase "Resultados (114).dta"
erase "Resultados (115).dta"
erase "Resultados (116).dta"
erase "Resultados (117).dta"
erase "Resultados (118).dta"
erase "Resultados (119).dta"
erase "Resultados (120).dta"
erase "Resultados (121).dta"
erase "Resultados (122).dta"
erase "Resultados (123).dta"
erase "Resultados (124).dta"
erase "Resultados (125).dta"
erase "Resultados (126).dta"
erase "Resultados (127).dta"
erase "Resultados (128).dta"
erase "Resultados (129).dta"
erase "Resultados (130).dta"
erase "Resultados (131).dta"
erase "Resultados (132).dta"
erase "Resultados (133).dta"
erase "Resultados (134).dta"
erase "Resultados (135).dta"
erase "Resultados (136).dta"
erase "Resultados (137).dta"
erase "Resultados (138).dta"
erase "Resultados (139).dta"
erase "Resultados (140).dta"
erase "Resultados (141).dta"
erase "Resultados (142).dta"
erase "Resultados (143).dta"
erase "Resultados (144).dta"
erase "Resultados (145).dta"
erase "Resultados (146).dta"
erase "Resultados (147).dta"
erase "Resultados (148).dta"
erase "Resultados (149).dta"
erase "Resultados (150).dta"
erase "Resultados (151).dta"
erase "Resultados (152).dta"
erase "Resultados (153).dta"
erase "Resultados (154).dta"
erase "Resultados (155).dta"
erase "Resultados (156).dta"
erase "Resultados (157).dta"
erase "Resultados (158).dta"
erase "Resultados (159).dta"
erase "Resultados (160).dta"
erase "Resultados (161).dta"
erase "Resultados (162).dta"
erase "Resultados (164).dta"
erase "Resultados (165).dta"
erase "Resultados (166).dta"
erase "Resultados (167).dta"
erase "Resultados (168).dta"
erase "Resultados (169).dta"
erase "Resultados (171).dta"
erase "Resultados (172).dta"
erase "Resultados (173).dta"
erase "Resultados (174).dta"
erase "Resultados (175).dta"
erase "Resultados (176).dta"
erase "Resultados (177).dta"
erase "Resultados (178).dta"
