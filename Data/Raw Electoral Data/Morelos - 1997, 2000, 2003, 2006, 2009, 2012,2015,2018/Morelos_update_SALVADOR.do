////////////////////////////////////////////
////////MORELOS PRECINCT PANEL UPDATE///////
////////JUN 2019////////////////////////////
////////SALVADOR ASCENCIO///////////////////
////////////////////////////////////////////

cd "D:\Dropbox\Salvador Ascencio\Update Precincts\Morelos"

import excel "Ayuntamientos_2015.xlsx", firstrow clear

replace PRI=. if C_PRI_PVEM_PANAL!=.
replace PVEM=. if C_PRI_PVEM_PANAL!=.
replace PANAL=. if C_PRI_PVEM_PANAL!=.

rename C_PRI_PVEM_PANAL PRI_PVEM_PANAL

replace PRD=. if C_PRD_PT!=.
replace PT=. if C_PRD_PT!=.
rename C_PRD_PT PRD_PT

collapse (sum) PAN-CI_1 total listanominal, by (municipality section uniqueid)

egen valid = rowtotal(PAN PRI PRD PT PVEM MC PANAL PSD MORENA PES PH PRD_PT PRI_PVEM_PANAL CI_1)

foreach var in PAN PRI PRD PT PVEM MC PANAL PSD MORENA PES PH PRD_PT PRI_PVEM_PANAL CI_1 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

gen turnout=total/listanominal
gen mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PT i_PVEM i_MC i_PANAL i_PSD i_MORENA i_PES i_PH i_PRD_PT i_PRI_PVEM_PANAL i_CI_1, gen( PAN_r PRI_r PRD_r PT_r PVEM_r MC_r PANAL_r PSD_r MORENA_r PES_r PH_r PRD_PT_r PRI_PVEM_PANAL_r CI_1_r )
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM MC PANAL PSD MORENA PES PH PRD_PT PRI_PVEM_PANAL CI_1 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" 
replace second="Independent" if second=="CI_1"  
replace third="Independent" if third=="CI_1" 

gen  year=2015
gen month="June"
gen STATE="MORELOS"

save Morelos_Section_2015.dta, replace

collapse (first) winner, by (municipality uniqueid)
rename winner incumbent
save incumbents2018.dta, replace 

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

use Morelos_Section_2015.dta
collapse (sum) PAN-valid (first) STATE year month winner second third mun_turnout , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Morelos_2015.dta", replace

*********************************************************************************************************************************************
*********************************************************************************************************************************************
*********************************************************************************************************************************************

///////////////////////
////////2018///////////
///////////////////////

*See auxiliary file for Michoacan for the process to unify xlsx files
import excel using "Ayuntamientos_2018.xlsx", describe
forvalues sheet=1/`=r(N_worksheet)' {
	clear
	local sheetname=r(worksheet_`sheet')
	import excel "Ayuntamientos_2018.xlsx", sheet("`sheetname'") clear firstrow allstring
	drop if uniqueid==""
	foreach x of varlist * {
		replace `x' = "0" if `x'==""
	}
	save "`sheetname'.dta", replace
}

clear
append using "reporte (1).dta"
append using "reporte (2).dta"
append using "reporte (3).dta"
append using "reporte (4).dta"
append using "reporte (5).dta"
append using "reporte (6).dta"
append using "reporte (7).dta"
append using "reporte (8).dta"
append using "reporte (9).dta"
append using "reporte (10).dta"
append using "reporte (11).dta"
append using "reporte (12).dta"
append using "reporte (13).dta"
append using "reporte (14).dta"
append using "reporte (15).dta"
append using "reporte (16).dta"
append using "reporte (17).dta"
append using "reporte (18).dta"
append using "reporte (19).dta"
append using "reporte (20).dta"
append using "reporte (21).dta"
append using "reporte (22).dta"
append using "reporte (23).dta"
append using "reporte (24).dta"
append using "reporte (25).dta"
append using "reporte (26).dta"
append using "reporte (27).dta"
append using "reporte (28).dta"
append using "reporte (29).dta"
append using "reporte (30).dta"
append using "reporte (31).dta"
append using "reporte (32).dta"
append using "reporte (33).dta"

destring *, replace

rename votos* *

drop Z AA AB AC AF Y

rename (pan pri prd pvem pt mc na psd morena es humanista) (PAN PRI PRD PVEM PT MC PANAL PSD MORENA PES PH)
	
rename (no_registrados num_votos_nulos TotaldeVotos ListaNominal)(no_reg nulo total listanominal)

g PRD_PVEM_PSD = .
replace PRD_PVEM_PSD = cc_prd_pvem_psd + PRD + PSD + PVEM + config_cc_prd_pvem + config_cc_prd_psd + config_cc_pvem_psd if cc_prd_pvem_psd!=.
replace PRD=. if cc_prd_pvem_psd!=.
replace PVEM=. if cc_prd_pvem_psd!=.
replace PSD=. if cc_prd_pvem_psd!=.
drop config_cc_prd_pvem config_cc_prd_psd config_cc_pvem_psd

gen PT_MORENA_PES=.
replace PT_MORENA_PES=cc_pt_morena_pes + config_cc_pt_morena + config_cc_pt_pes + config_cc_morena_pes + MORENA + PT + PES if cc_pt_morena_pes!=.
replace MORENA=. if cc_pt_morena_pes!=.
replace PES=. if cc_pt_morena_pes!=.
replace PT=. if cc_pt_morena_pes!=.
replace PT_MORENA_PES=coal_pt_morena_pes + config_coal_pt_morena + config_coal_pt_pes + config_coal_morena_pes + MORENA + PT + PES if coal_pt_morena_pes!=.
replace MORENA=. if coal_pt_morena_pes!=.
replace PES=. if coal_pt_morena_pes!=.
replace PT=. if coal_pt_morena_pes!=.
drop config_cc_pt_morena config_cc_pt_pes config_cc_morena_pes coal_pt_morena_pes config_coal_pt_morena config_coal_pt_pes config_coal_morena_pes

gen PAN_MC=.
replace PAN_MC = cc_pan_mc + PAN + MC if cc_pan_mc!=.
replace PAN=. if cc_pan_mc!=.
replace MC=. if cc_pan_mc!=.

gen PRD_PSD=.
replace PRD_PSD = coal_prd_psd + PRD + PSD if coal_prd_psd!=.
replace PRD=. if coal_prd_psd!=.
replace PSD=. if coal_prd_psd!=.

gen coalpanmc=PAN_MC!=.
gen coalprdpsd=PRD_PSD!=.
gen coalpbt=PT_MORENA_PES!=.
gen coalprdpvempsd=PRD_PVEM_PSD!=.
drop cc_pan_mc coal_prd_psd cc_pt_morena_pes cc_prd_pvem_psd

rename cand_ind_* *

gen CI_1=0
replace CI_1=luis_granados_a_c + fuerza_independie + todos_juntos_por_ + ptix_a_c  + zeus_el_destructo  

gen CI_2=0
replace CI_2=juntos_para_atlat + armando_comunidad + raul_aguirre_espi + sumando_amigos_su + unidos_para_recup

gen CI_3=0
replace CI_3=juntos_a_renovar_ + accion_positiva_p + juntos_somos_la_s

drop luis_granados_a_c juntos_para_atlat juntos_a_renovar_ ///
     fuerza_independie armando_comunidad accion_positiva_p ///
	 todos_juntos_por_ raul_aguirre_espi ///
	 ptix_a_c sumando_amigos_su juntos_somos_la_s ///
	 zeus_el_destructo unidos_para_recup
	 
replace CI_1= CI_1 + es_el_tiempo_de_l + ayala_de_mis_amor + todos_somos_coatl + carlos_bildmart_b + ciudadanos_por_za + unidos_por_huitzi + reconstruccion_hu + movimiento_indepe + juntos_por_jonaca + miguel_angel_tova + por_un_futuro_par + tlahuica_independ + genaro_medina_a_c + tu_tienes_el_pode + somos_diferentes_ + del_pueblo_para_e

drop es_el_tiempo_de_l ayala_de_mis_amor todos_somos_coatl carlos_bildmart_b ciudadanos_por_za ///
    unidos_por_huitzi reconstruccion_hu movimiento_indepe juntos_por_jonaca miguel_angel_tova ///
	por_un_futuro_par tlahuica_independ genaro_medina_a_c tu_tienes_el_pode somos_diferentes_ del_pueblo_para_e
	
order PRD_PVEM_PSD PT_MORENA_PES PAN_MC PRD_PSD CI_*, b(no_reg)

collapse (sum) PAN-CI_3 total listanominal (first) coalpanmc coalprdpsd coalpbt coalprdpvempsd, by(municipality section uniqueid)

egen valid=rowtotal(PAN PRI PRD PVEM PT MC PANAL PSD MORENA PES PH PRD_PVEM_PSD PT_MORENA_PES PAN_MC PRD_PSD CI_1 CI_2 CI_3)

foreach var in PAN PRI PRD PVEM PT MC PANAL PSD MORENA PES PH PRD_PVEM_PSD PT_MORENA_PES PAN_MC PRD_PSD CI_1 CI_2 CI_3 total valid listanominal {
	bys uniqueid: egen mun_`var'= sum(`var') 
	g i_`var'= 1/mun_`var'
}

g turnout=total/listanominal
g mun_turnout=mun_total/mun_listanominal

rowranks i_PAN i_PRI i_PRD i_PVEM i_PT i_MC i_PANAL i_PSD i_MORENA i_PES i_PH i_PRD_PVEM_PSD i_PT_MORENA_PES i_PAN_MC i_PRD_PSD i_CI_1 i_CI_2 i_CI_3, ///
	gen(PAN_r PRI_r PRD_r PVEM_r PT_r MC_r PANAL_r PSD_r MORENA_r PES_r PH_r PRD_PVEM_PSD_r PT_MORENA_PES_r PAN_MC_r PRD_PSD_r CI_1_r CI_2_r CI_3_r)
drop i_*

gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PVEM PT MC PANAL PSD MORENA PES PH PRD_PVEM_PSD PT_MORENA_PES PAN_MC PRD_PSD CI_1 CI_2 CI_3 {
	replace winner = "`var'" if `var'_r == 1
	replace second = "`var'" if `var'_r == 2
	replace third = "`var'" if `var'_r == 3
}
drop *_r

replace winner="Independent" if winner=="CI_1" |  winner=="CI_2" | winner=="CI_3" 
replace second="Independent" if second=="CI_1"  | second=="CI_2"  | second=="CI_3" 
replace third="Independent" if third=="CI_1" | third=="CI_2" | third=="CI_3" 

g year=2018
g month="July"
g STATE="MORELOS"

save Morelos_Section_2018.dta, replace

merge m:1 uniqueid using incumbents2018.dta
drop _merge
erase incumbents2018.dta

g incumbent_vote = .

replace incumbent_vote=MC if incumbent=="MC" & coalpanmc==0
replace incumbent_vote=PAN if incumbent=="PAN" & coalpanmc==0
replace incumbent_vote=PAN_MC if (incumbent=="PAN" | incumbent=="MC") & coalpanmc>0

replace incumbent_vote=MORENA if incumbent=="MORENA" & coalpbt==0
replace incumbent_vote=PES if incumbent=="PES" & coalpbt==0
replace incumbent_vote=PT_MORENA_PES if (incumbent=="PES" | incumbent=="MORENA") & coalpbt>0

replace incumbent_vote=PH if incumbent=="PH"

replace incumbent_vote=PRI if incumbent=="PRI_PVEM_PANAL" 
replace incumbent_vote=PRD_PVEM_PSD if uniqueid==17011
replace incumbent_vote=PVEM if uniqueid==17024

replace incumbent_vote=PSD if incumbent=="PSD" & coalprdpsd==0 & coalprdpvempsd==0
replace incumbent_vote=PRD_PSD if incumbent=="PSD" & coalprdpsd>0
replace incumbent_vote=PRD_PVEM_PSD if incumbent=="PSD" & coalprdpvempsd>0

drop coal*

save Morelos_Section_2018.dta, replace

**********************************************************************
/////GEN MUNICIPAL DB
**********************************************************************

collapse (sum) PAN-valid incumbent_vote (first) STATE year month winner second third mun_turnout incumbent , by (municipality uniqueid)
rename mun_turnout turnout
sort uniqueid
order STATE municipality uniqueid * 
save "..\..\Update Municipal\Morelos_2018.dta", replace

*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************
*************************************************************************************************************************************************************************************

clear
append using Morelos_Section_2018.dta
append using Morelos_Section_2015.dta

erase Morelos_Section_2018.dta
erase Morelos_Section_2015.dta

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL MORENA MC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Morelos_Section_15_18.dta, replace

use "..\..\Precinct\Morelos_ALL.dta", clear
append using Morelos_Section_15_18.dta
save Morelos_ALL_SALVADOR.dta, replace 
	
erase Morelos_Section_15_18.dta

erase "reporte (1).dta"
erase "reporte (2).dta"
erase "reporte (3).dta"
erase "reporte (4).dta"
erase "reporte (5).dta"
erase "reporte (6).dta"
erase "reporte (7).dta"
erase "reporte (8).dta"
erase "reporte (9).dta"
erase "reporte (10).dta"
erase "reporte (11).dta"
erase "reporte (12).dta"
erase "reporte (13).dta"
erase "reporte (14).dta"
erase "reporte (15).dta"
erase "reporte (16).dta"
erase "reporte (17).dta"
erase "reporte (18).dta"
erase "reporte (19).dta"
erase "reporte (20).dta"
erase "reporte (21).dta"
erase "reporte (22).dta"
erase "reporte (23).dta"
erase "reporte (24).dta"
erase "reporte (25).dta"
erase "reporte (26).dta"
erase "reporte (27).dta"
erase "reporte (28).dta"
erase "reporte (29).dta"
erase "reporte (30).dta"
erase "reporte (31).dta"
erase "reporte (32).dta"
erase "reporte (33).dta"
	
