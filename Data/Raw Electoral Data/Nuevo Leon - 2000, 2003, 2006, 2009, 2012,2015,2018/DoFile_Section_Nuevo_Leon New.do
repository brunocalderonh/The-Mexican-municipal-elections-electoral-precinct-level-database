
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Nuevo Leon - 2000, 2003, 2006, 2009, 2012"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Nuevo Leon - 2000, 2003, 2006, 2009, 2012"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2000.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  total listanominal, replace

collapse (sum)  pan -  total listanominal, by (municipality section)

rename pan PAN
rename pri PRI
* Alianza: PRD-Convergencia-PPN-PSN-PAS
rename alianza  PRD_PC_PPN_PSN_PAS
rename pt  PT
rename pvem PVEM
* Partido de Centro Democrático
rename pcd PCD
rename parm PARM
* Partido Democracia Social
rename ds PDS

gen turnout =  total/listanominal

drop   votosnulos 

gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANAHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMENEZ"
replace uniqueid=19010 if municipality =="EL CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIENEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZALEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCIA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERAN"
replace uniqueid=19023 if municipality =="GRAL. TREVIÑO"
replace uniqueid=19024 if municipality =="GENERAL ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUAREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARIN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARAS"
replace uniqueid=19041 if municipality =="PESQUERIA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLÁS DE LOS GARZA"
replace uniqueid=19019 if municipality =="GARZA GARCIA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"

egen valid = rowtotal(PAN PRI PRD_PC_PPN_PSN_PAS PT PVEM PCD PARM PDS)

foreach var in PAN PRI PRD_PC_PPN_PSN_PAS PT PVEM PCD PARM PDS total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal


rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PC_PPN_PSN_PAS inv_mun_PT inv_mun_PVEM inv_mun_PCD inv_mun_PARM inv_mun_PDS, gen(PAN_r PRI_r PRD_PC_PPN_PSN_PAS_r PT_r PVEM_r PCD_r PARM_r PDS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PC_PPN_PSN_PAS PT PVEM PCD PARM PDS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2000
gen month ="July"

save Nuevo_Leon_Section_2000.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2003.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan -  total , replace

collapse (sum)  listanominal pan -  mexicoposible total , by (municipality section)


**************************************************************************
* "Alianza Ciudadana" PRI-PVEM-PLM-FC(PPN) or PRI-PVEM-PLM-FC(PPN)-PT
gen pri_pvem_plm_fc = 0
replace pri_pvem_plm_fc = ca if pt>0

gen pri_pvem_pt_plm_fc = 0
replace pri_pvem_pt_plm_fc = ca  if pt==0 

count if pri_pvem_plm_fc + pri_pvem_pt_plm_fc!=ca
drop ca 

**************************************************************************

rename pan PAN
rename pri_pvem_plm_fc PRI_PVEM_PLM_FC
rename pri_pvem_pt_plm_fc PRI_PT_PVEM_PLM_FC
rename prd PRD
rename pt  PT
rename psn PSN
rename pas PAS
rename convergencia PC
rename mexicoposible MexicoPosible

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANÁHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMÉNEZ"
replace uniqueid=19010 if municipality =="EL CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIÉNEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZÁLEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCÍA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERÁN"
replace uniqueid=19023 if municipality =="GRAL. TREVIÑO"
replace uniqueid=19024 if municipality =="GRAL. ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUÁREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARÍN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARÁS"
replace uniqueid=19041 if municipality =="PESQUERÍA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLÁS DE LOS GARZA"
replace uniqueid=19019 if municipality =="SAN PEDRO GARZA GARCÍA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"

egen valid = rowtotal(PAN PRD PT PSN PAS PC MexicoPosible PRI_PVEM_PLM_FC PRI_PT_PVEM_PLM_FC)

foreach var in PAN PRD PT PSN PAS PC MexicoPosible PRI_PVEM_PLM_FC PRI_PT_PVEM_PLM_FC total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRD inv_mun_PT inv_mun_PSN inv_mun_PAS inv_mun_PC inv_mun_MexicoPosible inv_mun_PRI_PVEM_PLM_FC inv_mun_PRI_PT_PVEM_PLM_FC, gen(PAN_r PRD_r PT_r PSN_r PAS_r PC_r MexicoPosible_r PRI_PVEM_PLM_FC_r PRI_PT_PVEM_PLM_FC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRD PT PSN PAS PC MexicoPosible PRI_PVEM_PLM_FC PRI_PT_PVEM_PLM_FC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2003
gen month ="July"

sort section

save Nuevo_Leon_Section_2003.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename nombre_municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal  nulos -  total , replace

collapse (sum)  listanominal   pan -   total , by (municipality section)

**************************************************************************

gen prdpt =  0
replace prdpt = prdptpc if convergencia>0
replace prdptpc = 0 if convergencia>0

**************************************************************************

rename pan PAN
rename pripvem PRI_PVEM
rename prdptpc PRD_PT_PC
rename prdpt PRD_PT
rename convergencia  PC
rename pr PartidoRepublicano
rename alternativa PAS
rename nuevaalianza PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=19001 if municipality =="Abasolo"
replace uniqueid=19002 if municipality =="Agualeguas"
replace uniqueid=19004 if municipality =="Allende"
replace uniqueid=19005 if municipality =="An¿huac"
replace uniqueid=19006 if municipality =="Apodaca"
replace uniqueid=19007 if municipality =="Aramberri"
replace uniqueid=19008 if municipality =="Bustamante"
replace uniqueid=19009 if municipality =="Cadereyta Jim¿nez"
replace uniqueid=19010 if municipality =="El Carmen"
replace uniqueid=19011 if municipality =="Cerralvo"
replace uniqueid=19013 if municipality =="China"
replace uniqueid=19012 if municipality =="Ci¿nega de Flores"
replace uniqueid=19014 if municipality =="Dr. Arroyo"
replace uniqueid=19015 if municipality =="Dr. Coss"
replace uniqueid=19016 if municipality =="Dr. Gonz¿lez"
replace uniqueid=19017 if municipality =="Galeana"
replace uniqueid=19018 if municipality =="Garc¿a"
replace uniqueid=19020 if municipality =="Gral. Bravo"
replace uniqueid=19021 if municipality =="Gral. Escobedo"
replace uniqueid=19022 if municipality =="Gral. Ter¿n"
replace uniqueid=19023 if municipality =="Gral. Trevi¿o"
replace uniqueid=19024 if municipality =="Gral. Zaragoza"
replace uniqueid=19025 if municipality =="Gral. Zuazua"
replace uniqueid=19026 if municipality =="Guadalupe"
replace uniqueid=19047 if municipality =="Hidalgo"
replace uniqueid=19028 if municipality =="Higueras"
replace uniqueid=19029 if municipality =="Hualahuises"
replace uniqueid=19030 if municipality =="Iturbide"
replace uniqueid=19031 if municipality =="Ju¿rez"
replace uniqueid=19032 if municipality =="Lampazos de Naranjo"
replace uniqueid=19033 if municipality =="Linares"
replace uniqueid=19003 if municipality =="Los Aldamas"
replace uniqueid=19027 if municipality =="Los Herreras"
replace uniqueid=19042 if municipality =="Los Ramones"
replace uniqueid=19034 if municipality =="Mar¿n"
replace uniqueid=19035 if municipality =="Melchor Ocampo"
replace uniqueid=19036 if municipality =="Mier y Noriega"
replace uniqueid=19037 if municipality =="Mina"
replace uniqueid=19038 if municipality =="Montemorelos"
replace uniqueid=19039 if municipality =="Monterrey"
replace uniqueid=19040 if municipality =="Par¿s"
replace uniqueid=19041 if municipality =="Pesquer¿a"
replace uniqueid=19043 if municipality =="Rayones"
replace uniqueid=19044 if municipality =="Sabinas Hidalgo"
replace uniqueid=19045 if municipality =="Salinas Victoria"
replace uniqueid=19046 if municipality =="San Nicol¿s de los Garza"
replace uniqueid=19019 if municipality =="San Pedro Garza Garc¿a"
replace uniqueid=19048 if municipality =="Santa Catarina"
replace uniqueid=19049 if municipality =="Santiago"
replace uniqueid=19050 if municipality =="Vallecillo"
replace uniqueid=19051 if municipality =="Villaldama"

egen valid = rowtotal(PAN PRI_PVEM PRD_PT_PC PC PartidoRepublicano PAS PANAL PRD_PT)

foreach var in PAN PRI_PVEM PRD_PT_PC PC PartidoRepublicano PAS PANAL PRD_PT total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD_PT_PC inv_mun_PC inv_mun_PartidoRepublicano inv_mun_PAS inv_mun_PANAL inv_mun_PRD_PT, gen(PAN_r PRI_PVEM_r PRD_PT_PC_r PC_r PartidoRepublicano_r PAS_r PANAL_r PRD_PT_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM PRD_PT_PC PC PartidoRepublicano PAS PANAL PRD_PT {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2006
gen month ="July"

sort section

save Nuevo_Leon_Section_2006.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  listanominal , replace

collapse (sum)  pan - panal total listanominal , by (municipality section)

* pripvempdpcc (PRI, PVEM, PD, Cruzada Ciudadana) 

rename pan PAN
rename pripvempdpcc PRI_PVEM_PD_CC
rename prd PRD
rename prdpsd  PRD_PSD
rename pt PT
rename conv PC
rename psd PSD
rename panal PANAL

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANAHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMENEZ"
replace uniqueid=19010 if municipality =="CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIENEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZALEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCIA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERAN"
replace uniqueid=19023 if municipality =="GRAL. TREVIÑO"
replace uniqueid=19024 if municipality =="GRAL. ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUAREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARIN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARAS"
replace uniqueid=19041 if municipality =="PESQUERIA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLAS DE LOS GARZA"
replace uniqueid=19019 if municipality =="SAN PEDRO GARZA GARCIA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"


egen valid = rowtotal(PAN PRI_PVEM_PD_CC PRD PRD_PSD PT PC PSD PANAL)

foreach var in PAN PRI_PVEM_PD_CC PRD PRD_PSD PT PC PSD PANAL total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM_PD_CC inv_mun_PRD inv_mun_PRD_PSD inv_mun_PT inv_mun_PC inv_mun_PSD inv_mun_PANAL, gen(PAN_r PRI_PVEM_PD_CC_r PRD_r PRD_PSD_r PT_r PC_r PSD_r PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM_PD_CC PRD PRD_PSD PT PC PSD PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2009
gen month ="July"

sort section

save Nuevo_Leon_Section_2009.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

import excel "Ayu_Seccion_2012.xlsx", sheet("Sheet1") firstrow clear

rename Municipality municipality
rename Seccin section

drop if municipality==""
destring *, replace

drop if total==. | total==0 

collapse (sum)  PAN - PANAL  total listanominal , by(municipality section)

gen turnout =  total/listanominal

replace municipality= upper(municipality)

gen   uniqueid= 0
replace uniqueid=19001 if municipality =="ABASOLO"
replace uniqueid=19002 if municipality =="AGUALEGUAS"
replace uniqueid=19004 if municipality =="ALLENDE"
replace uniqueid=19005 if municipality =="ANAHUAC"
replace uniqueid=19006 if municipality =="APODACA"
replace uniqueid=19007 if municipality =="ARAMBERRI"
replace uniqueid=19008 if municipality =="BUSTAMANTE"
replace uniqueid=19009 if municipality =="CADEREYTA JIMENEZ"
replace uniqueid=19010 if municipality =="EL CARMEN"
replace uniqueid=19011 if municipality =="CERRALVO"
replace uniqueid=19013 if municipality =="CHINA"
replace uniqueid=19012 if municipality =="CIENEGA DE FLORES"
replace uniqueid=19014 if municipality =="DR. ARROYO"
replace uniqueid=19015 if municipality =="DR. COSS"
replace uniqueid=19016 if municipality =="DR. GONZALEZ"
replace uniqueid=19017 if municipality =="GALEANA"
replace uniqueid=19018 if municipality =="GARCIA"
replace uniqueid=19020 if municipality =="GRAL. BRAVO"
replace uniqueid=19021 if municipality =="GRAL. ESCOBEDO"
replace uniqueid=19022 if municipality =="GRAL. TERAN"
replace uniqueid=19023 if municipality =="GRAL. TREVINO"
replace uniqueid=19024 if municipality =="GRAL. ZARAGOZA"
replace uniqueid=19025 if municipality =="GRAL. ZUAZUA"
replace uniqueid=19026 if municipality =="GUADALUPE"
replace uniqueid=19047 if municipality =="HIDALGO"
replace uniqueid=19028 if municipality =="HIGUERAS"
replace uniqueid=19029 if municipality =="HUALAHUISES"
replace uniqueid=19030 if municipality =="ITURBIDE"
replace uniqueid=19031 if municipality =="JUAREZ"
replace uniqueid=19032 if municipality =="LAMPAZOS DE NARANJO"
replace uniqueid=19033 if municipality =="LINARES"
replace uniqueid=19003 if municipality =="LOS ALDAMAS"
replace uniqueid=19027 if municipality =="LOS HERRERAS"
replace uniqueid=19042 if municipality =="LOS RAMONES"
replace uniqueid=19034 if municipality =="MARIN"
replace uniqueid=19035 if municipality =="MELCHOR OCAMPO"
replace uniqueid=19036 if municipality =="MIER Y NORIEGA"
replace uniqueid=19037 if municipality =="MINA"
replace uniqueid=19038 if municipality =="MONTEMORELOS"
replace uniqueid=19039 if municipality =="MONTERREY"
replace uniqueid=19040 if municipality =="PARAS"
replace uniqueid=19041 if municipality =="PESQUERIA"
replace uniqueid=19043 if municipality =="RAYONES"
replace uniqueid=19044 if municipality =="SABINAS HIDALGO"
replace uniqueid=19045 if municipality =="SALINAS VICTORIA"
replace uniqueid=19046 if municipality =="SAN NICOLAS DE LOS GARZA"
replace uniqueid=19019 if municipality =="SAN PEDRO GARZA GARCIA"
replace uniqueid=19048 if municipality =="SANTA CATARINA"
replace uniqueid=19049 if municipality =="SANTIAGO"
replace uniqueid=19050 if municipality =="VALLECILLO"
replace uniqueid=19051 if municipality =="VILLALDAMA"


egen valid = rowtotal(PAN PRI_PVEM PRD PT PC PANAL)

foreach var in PAN PRI_PVEM PRD PT PC PANAL total listanominal valid {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI_PVEM inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PANAL, gen(PAN_r PRI_PVEM_r PRD_r PT_r PC_r PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI_PVEM PRD PT PC PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2012
gen month ="July"

sort section

save Nuevo_Leon_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Nuevo_Leon_Section_2000.dta
append using Nuevo_Leon_Section_2003.dta
append using Nuevo_Leon_Section_2006.dta
append using Nuevo_Leon_Section_2009.dta
append using Nuevo_Leon_Section_2012.dta

erase Nuevo_Leon_Section_2000.dta
erase Nuevo_Leon_Section_2003.dta
erase Nuevo_Leon_Section_2006.dta
erase Nuevo_Leon_Section_2009.dta
erase Nuevo_Leon_Section_2012.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PVEM PANAL PD PSD PAS PartidoMexicoPosible PPN PSN PLM FC CC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Nuevo_Leon_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Nuevo_Leon_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
