
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Tamaulipas - 1995, 1998, 2001, 2004, 2007, 2010, 2013"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1998_No_LN.csv, clear

rename municipio  municipality
rename secc section
* rename listanominal nominal

drop if municipality=="" & section==.
* drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PartidoCardenista 
rename parm PARM

* gen turnout =  total/listanominal

drop  nulos  noreg 

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="ABASOLO"
replace uniqueid=28002 if municipality =="ALDAMA"
replace uniqueid=28003 if municipality =="ALTAMIRA"
replace uniqueid=28004 if municipality =="ANT. MORELOS"
replace uniqueid=28005 if municipality =="BURGOS"
replace uniqueid=28006 if municipality =="BUSTAMANTE"
replace uniqueid=28007 if municipality =="CAMARGO"
replace uniqueid=28008 if municipality =="CASAS"
replace uniqueid=28009 if municipality =="CD. MADERO"
replace uniqueid=28010 if municipality =="CRUILLAS"
replace uniqueid=28021 if municipality =="MANTE"
replace uniqueid=28011 if municipality =="GOMEZ FARIAS"
replace uniqueid=28012 if municipality =="GONZALEZ"
replace uniqueid=28013 if municipality =="GUEMEZ"
replace uniqueid=28014 if municipality =="GUERRERO"
replace uniqueid=28015 if municipality =="G DIAZ ORDAZ"
replace uniqueid=28016 if municipality =="HIDALGO"
replace uniqueid=28017 if municipality =="JAUMAVE"
replace uniqueid=28018 if municipality =="JIMENEZ"
replace uniqueid=28019 if municipality =="LLERA"
replace uniqueid=28020 if municipality =="MAINERO"
replace uniqueid=28022 if municipality =="MATAMOROS"
replace uniqueid=28023 if municipality =="MENDEZ"
replace uniqueid=28024 if municipality =="MIER"
replace uniqueid=28025 if municipality =="MIGUEL ALEMAN"
replace uniqueid=28026 if municipality =="MIQUIHUANA"
replace uniqueid=28027 if municipality =="NVO. LAREDO"
replace uniqueid=28028 if municipality =="NVO. MORELOS"
replace uniqueid=28029 if municipality =="OCAMPO"
replace uniqueid=28030 if municipality =="PADILLA"
replace uniqueid=28031 if municipality =="PALMILLA"
replace uniqueid=28032 if municipality =="REYNOSA"
replace uniqueid=28033 if municipality =="RIO BRAVO"
replace uniqueid=28034 if municipality =="SAN CARLOS"
replace uniqueid=28035 if municipality =="SAN FERNANDO"
replace uniqueid=28036 if municipality =="SAN NICOLAS"
replace uniqueid=28037 if municipality =="SOTO LA MARINA"
replace uniqueid=28038 if municipality =="TAMPICO"
replace uniqueid=28039 if municipality =="TULA"
replace uniqueid=28040 if municipality =="VALLE HERMOSO"
replace uniqueid=28041 if municipality =="VICTORIA"
replace uniqueid=28042 if municipality =="VILLAGRAN"
replace uniqueid=28043 if municipality =="XICOTENCATL"

egen valid = rowtotal(PAN PRI PRD PT PVEM PARM PartidoCardenista)

foreach var in PAN PRI PRD PT PVEM PARM PartidoCardenista total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PARM inv_mun_PartidoCardenista, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PARM_r PartidoCardenista_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PARM" if PARM_r ==1 
replace winner = "PartidoCardenista" if PartidoCardenista_r ==1 
drop *_r

gen year = 1998
gen month ="October"

save Tamaulipas_Section_1998.dta, replace

keep municipality section uniqueid
sort section

save Tamaulipas_Section_1998_to_Merge_1995.dta, replace

use Tamaulipas_Section_1998.dta, clear

drop if total==. | total==0 

save Tamaulipas_Section_1998.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1995_No_Municipalities_No_LN.csv, clear

rename seccion section
* rename listanominal nominal
drop if section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pfcrn   PartidoCardenista 
rename parm  PARM

* gen turnout =  total/nominal

drop  nulos validos  

sort section

merge section using Tamaulipas_Section_1998_to_Merge_1995.dta
drop if _merge==2
drop _merge

egen valid = rowtotal(PAN PRI PRD PartidoCardenista PARM PT PVEM)

foreach var in PAN PRI PRD PartidoCardenista PARM PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartidoCardenista inv_mun_PARM inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_r PartidoCardenista_r PARM_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PARM" if PARM_r ==1 
replace winner = "PartidoCardenista" if PartidoCardenista_r ==1 
drop *_r

gen year = 1995
gen month ="November"

save Tamaulipas_Section_1995.dta, replace




**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2001_No_LN.csv, clear

rename municipio  municipality
rename secc section
drop if municipality=="" & section==.
drop if total ==. | total ==0

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

***************************************************************************

drop prdpvem
* MANTE : PT-PVEM
replace ptpvem = ptpvem + pt + pvem if municipality=="MANTE"
replace pt   = 0 if municipality=="MANTE"
replace pvem = 0 if municipality=="MANTE"

* REYNOSA : PAS-PVEM-PT
replace paspvempt = paspvempt + pas + pvem + pt if municipality=="REYNOSA"
replace pas = 0 if municipality=="REYNOSA"
replace pvem = 0 if municipality=="REYNOSA"
replace pt = 0 if municipality=="REYNOSA"

***************************************************************************

replace total=. if total==0 

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem PVEM
rename pc   PC
rename pas  PAS
rename psn  PSN
rename ptpvem  PT_PVEM
rename paspvempt  PAS_PT_PVEM
 
* gen turnout =  total/listanominal

drop   validos nulos  

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="ABASOLO"
replace uniqueid=28002 if municipality =="ALDAMA"
replace uniqueid=28003 if municipality =="ALTAMIRA"
replace uniqueid=28004 if municipality =="A. MORELOS"
replace uniqueid=28005 if municipality =="BURGOS"
replace uniqueid=28006 if municipality =="BUSTAMANTE"
replace uniqueid=28007 if municipality =="CAMARGO"
replace uniqueid=28008 if municipality =="CASAS"
replace uniqueid=28009 if municipality =="CD. MADERO"
replace uniqueid=28010 if municipality =="CRUILLAS"
replace uniqueid=28021 if municipality =="MANTE"
replace uniqueid=28011 if municipality =="GOMEZ F."
replace uniqueid=28012 if municipality =="GONZALEZ"
replace uniqueid=28013 if municipality =="GUEMEZ"
replace uniqueid=28014 if municipality =="GUERRERO"
replace uniqueid=28015 if municipality =="GVO. DIAZ O."
replace uniqueid=28016 if municipality =="HIDALGO"
replace uniqueid=28017 if municipality =="JAUMAVE"
replace uniqueid=28018 if municipality =="JIMENEZ"
replace uniqueid=28019 if municipality =="LLERA"
replace uniqueid=28020 if municipality =="MAINERO"
replace uniqueid=28022 if municipality =="MATAMOROS"
replace uniqueid=28023 if municipality =="MENDEZ"
replace uniqueid=28024 if municipality =="MIER"
replace uniqueid=28025 if municipality =="MIGUEL A."
replace uniqueid=28026 if municipality =="MIQUIHUANA"
replace uniqueid=28027 if municipality =="NVO. LAREDO"
replace uniqueid=28028 if municipality =="NVO. MORELOS"
replace uniqueid=28029 if municipality =="OCAMPO"
replace uniqueid=28030 if municipality =="PADILLA"
replace uniqueid=28031 if municipality =="PALMILLAS"
replace uniqueid=28032 if municipality =="REYNOSA"
replace uniqueid=28033 if municipality =="RÍO BRAVO"
replace uniqueid=28034 if municipality =="SAN CARLOS"
replace uniqueid=28035 if municipality =="SAN FERNANDO"
replace uniqueid=28036 if municipality =="SAN NICOLAS"
replace uniqueid=28037 if municipality =="SOTO LA MARINA"
replace uniqueid=28038 if municipality =="TAMPICO"
replace uniqueid=28039 if municipality =="TULA"
replace uniqueid=28040 if municipality =="VALLE HERMOSO"
replace uniqueid=28041 if municipality =="VICTORIA"
replace uniqueid=28042 if municipality =="VILLAGRAN"
replace uniqueid=28043 if municipality =="XICOTENCATL"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PSN PAS PT_PVEM PAS_PT_PVEM)

foreach var in PAN PRI PRD PT PVEM PC PSN PAS PT_PVEM PAS_PT_PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PT_PVEM inv_mun_PAS_PT_PVEM, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PSN_r PAS_r PT_PVEM_r PAS_PT_PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PSN" if PSN_r ==1
replace winner = "PAS" if PAS_r ==1 
replace winner = "PT_PVEM" if PT_PVEM_r ==1 
replace winner = "PAS_PT_PVEM" if PAS_PT_PVEM_r ==1  
drop *_r

gen year = 2001
gen month ="October"

save Tamaulipas_Section_2001.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2004_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)
 
rename pan  PAN
rename pri  PRI
rename prdpc  PRD_PC
rename pt   PT
rename pvem PVEM

* gen turnout =  total/listanominal

drop   validos nulos  

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="ABASOLO"
replace uniqueid=28002 if municipality =="ALDAMA"
replace uniqueid=28003 if municipality =="ALTAMIRA"
replace uniqueid=28004 if municipality =="ANTIGUO MORELOS"
replace uniqueid=28005 if municipality =="BURGOS"
replace uniqueid=28006 if municipality =="BUSTAMANTE"
replace uniqueid=28007 if municipality =="CAMARGO"
replace uniqueid=28008 if municipality =="CASAS"
replace uniqueid=28009 if municipality =="CD. MADERO"
replace uniqueid=28010 if municipality =="CRUILLAS"
replace uniqueid=28021 if municipality =="MANTE"
replace uniqueid=28011 if municipality =="GÓMEZ FARIAS"
replace uniqueid=28012 if municipality =="GONZÁLEZ"
replace uniqueid=28013 if municipality =="GÜÉMEZ"
replace uniqueid=28014 if municipality =="GUERRERO"
replace uniqueid=28015 if municipality =="G. DIAZ ORDAZ"
replace uniqueid=28016 if municipality =="HIDALGO"
replace uniqueid=28017 if municipality =="JAUMAVE"
replace uniqueid=28018 if municipality =="JIMÉNEZ"
replace uniqueid=28019 if municipality =="LLERA"
replace uniqueid=28020 if municipality =="MAINERO"
replace uniqueid=28022 if municipality =="MATAMOROS"
replace uniqueid=28023 if municipality =="MENDEZ"
replace uniqueid=28024 if municipality =="MIER"
replace uniqueid=28025 if municipality =="MIGUEL ALEMAN"
replace uniqueid=28026 if municipality =="MIQUIHUANA"
replace uniqueid=28027 if municipality =="NUEVO LAREDO"
replace uniqueid=28028 if municipality =="NUEVO MORELOS"
replace uniqueid=28029 if municipality =="OCAMPO"
replace uniqueid=28030 if municipality =="PADILLA"
replace uniqueid=28031 if municipality =="PALMILLAS"
replace uniqueid=28032 if municipality =="REYNOSA"
replace uniqueid=28033 if municipality =="RIO BRAVO"
replace uniqueid=28034 if municipality =="SAN CARLOS"
replace uniqueid=28035 if municipality =="SAN FERNANDO"
replace uniqueid=28036 if municipality =="SAN NICOLAS"
replace uniqueid=28037 if municipality =="SOTO LA MARINA"
replace uniqueid=28038 if municipality =="TAMPICO"
replace uniqueid=28039 if municipality =="TULA"
replace uniqueid=28040 if municipality =="VALLE HERMOSO"
replace uniqueid=28041 if municipality =="VICTORIA"
replace uniqueid=28042 if municipality =="VILLAGRAN"
replace uniqueid=28043 if municipality =="XICOTENCATL"

egen valid = rowtotal(PAN PRI PRD_PC PT PVEM)

foreach var in PAN PRI PRD_PC PT PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PC inv_mun_PT inv_mun_PVEM, gen(PAN_r PRI_r PRD_PC_r PT_r PVEM_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD_PC" if PRD_PC_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
drop *_r

gen year = 2004
gen month ="November"

sort section

save Tamaulipas_Section_2004.dta, replace


**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2007.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan - total , replace

collapse (sum) listanominal pan - total , by (municipality section)
 
rename pan   PAN
rename pri   PRI
rename prd   PRD
rename pt    PT
rename pvem  PVEM
rename pc    PC
rename prdpt PRD_PT
rename pas   PAS

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="ABASOLO"
replace uniqueid=28002 if municipality =="ALDAMA"
replace uniqueid=28003 if municipality =="ALTAMIRA"
replace uniqueid=28004 if municipality =="ANTIGUO MORELOS"
replace uniqueid=28005 if municipality =="BURGOS"
replace uniqueid=28006 if municipality =="BUSTAMANTE"
replace uniqueid=28007 if municipality =="CAMARGO"
replace uniqueid=28008 if municipality =="CASAS"
replace uniqueid=28009 if municipality =="CIUDAD MADERO"
replace uniqueid=28010 if municipality =="CRUILLAS"
replace uniqueid=28021 if municipality =="EL MANTE"
replace uniqueid=28011 if municipality =="GOMEZ FARIAS"
replace uniqueid=28012 if municipality =="GONZALEZ"
replace uniqueid=28013 if municipality =="GUEMEZ"
replace uniqueid=28014 if municipality =="GUERRERO"
replace uniqueid=28015 if municipality =="GUSTAVO DIAZ ORDAZ"
replace uniqueid=28016 if municipality =="HIDALGO"
replace uniqueid=28017 if municipality =="JAUMAVE"
replace uniqueid=28018 if municipality =="JIMENEZ"
replace uniqueid=28019 if municipality =="LLERA"
replace uniqueid=28020 if municipality =="MAINERO"
replace uniqueid=28022 if municipality =="MATAMOROS"
replace uniqueid=28023 if municipality =="MENDEZ"
replace uniqueid=28024 if municipality =="MIER"
replace uniqueid=28025 if municipality =="MIGUEL ALEMAN"
replace uniqueid=28026 if municipality =="MIQUIHUANA"
replace uniqueid=28027 if municipality =="NUEVO LAREDO"
replace uniqueid=28028 if municipality =="NUEVO MORELOS"
replace uniqueid=28029 if municipality =="OCAMPO"
replace uniqueid=28030 if municipality =="PADILLA"
replace uniqueid=28031 if municipality =="PALMILLAS"
replace uniqueid=28032 if municipality =="REYNOSA"
replace uniqueid=28033 if municipality =="RIO BRAVO"
replace uniqueid=28034 if municipality =="SAN CARLOS"
replace uniqueid=28035 if municipality =="SAN FERNANDO"
replace uniqueid=28036 if municipality =="SAN NICOLAS"
replace uniqueid=28037 if municipality =="SOTO LA MARINA"
replace uniqueid=28038 if municipality =="TAMPICO"
replace uniqueid=28039 if municipality =="TULA"
replace uniqueid=28040 if municipality =="VALLE HERMOSO"
replace uniqueid=28041 if municipality =="VICTORIA"
replace uniqueid=28042 if municipality =="VILLAGRAN"
replace uniqueid=28043 if municipality =="XICOTENCATL"

egen valid = rowtotal(PAN PRI PRD PRD_PT PT PVEM PC PAS)

foreach var in PAN PRI PRD PRD_PT PT PVEM PC PAS listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PRD_PT inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PAS, gen(PAN_r PRI_r PRD_r PRD_PT_r PT_r PVEM_r PC_r PAS_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PRD_PT" if PRD_PT_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PC" if PC_r ==1 
replace winner = "PAS" if PAS_r ==1 
drop *_r

gen year = 2007
gen month ="November"

sort section

save Tamaulipas_Section_2007.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2010.csv, clear

rename municipio  municipality
rename seccin section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring listanominal pan - total , replace

collapse (sum) listanominal pan - total , by (municipality section)
   
rename pan   PAN
rename pri   PRI
rename pripanal   PRI_PANAL
rename pripvempanal   PRI_PVEM_PANAL
rename prd   PRD
rename pt    PT
rename pvem  PVEM
rename pc    PC
rename panal PANAL

gen turnout =  total/listanominal

drop    nulos validos  

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="Abasolo"
replace uniqueid=28002 if municipality =="Aldama"
replace uniqueid=28003 if municipality =="Altamira"
replace uniqueid=28004 if municipality =="Ant. Morelos"
replace uniqueid=28005 if municipality =="Burgos"
replace uniqueid=28006 if municipality =="Bustamante"
replace uniqueid=28007 if municipality =="Camargo"
replace uniqueid=28008 if municipality =="Casas"
replace uniqueid=28009 if municipality =="Cd.Madero"
replace uniqueid=28010 if municipality =="Cruillas"
replace uniqueid=28021 if municipality =="Mante, El"
replace uniqueid=28011 if municipality =="Gomez Farias"
replace uniqueid=28012 if municipality =="González"
replace uniqueid=28013 if municipality =="Guemez"
replace uniqueid=28014 if municipality =="Guerrero"
replace uniqueid=28015 if municipality =="G. Díaz Ordaz"
replace uniqueid=28016 if municipality =="Hidalgo"
replace uniqueid=28017 if municipality =="Jaumave"
replace uniqueid=28018 if municipality =="Jimenez"
replace uniqueid=28019 if municipality =="Llera"
replace uniqueid=28020 if municipality =="Mainero"
replace uniqueid=28022 if municipality =="Matamoros"
replace uniqueid=28023 if municipality =="Méndez"
replace uniqueid=28024 if municipality =="Mier"
replace uniqueid=28025 if municipality =="Miguel Aleman"
replace uniqueid=28026 if municipality =="Miquihuana"
replace uniqueid=28027 if municipality =="Nvo. Laredo"
replace uniqueid=28028 if municipality =="Nvo Morelos"
replace uniqueid=28029 if municipality =="Ocampo"
replace uniqueid=28030 if municipality =="Padilla"
replace uniqueid=28031 if municipality =="Palmillas"
replace uniqueid=28032 if municipality =="Reynosa"
replace uniqueid=28033 if municipality =="Río Bravo"
replace uniqueid=28034 if municipality =="San Carlos"
replace uniqueid=28035 if municipality =="San Fernando"
replace uniqueid=28036 if municipality =="San Nicolás"
replace uniqueid=28037 if municipality =="Soto la Marina"
replace uniqueid=28038 if municipality =="Tampico"
replace uniqueid=28039 if municipality =="Tula"
replace uniqueid=28040 if municipality =="Valle Hermoso"
replace uniqueid=28041 if municipality =="Victoria"
replace uniqueid=28042 if municipality =="Villagrán"
replace uniqueid=28043 if municipality =="Xicotencatl"

egen valid = rowtotal(PAN PRI PRI_PANAL PRI_PVEM_PANAL PRD PT PVEM PANAL PC)

foreach var in PAN PRI PRI_PANAL PRI_PVEM_PANAL PRD PT PVEM PANAL PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRI_PANAL inv_mun_PRI_PVEM_PANAL inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PANAL inv_mun_PC, gen(PAN_r PRI_r PRI_PANAL_r PRI_PVEM_PANAL_r PRD_r PT_r PVEM_r PANAL_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PANAL" if PRI_PANAL_r ==1 
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2010
gen month ="July"

sort section

save Tamaulipas_Section_2010.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2013.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" | section==.
drop if total==. | total==0 

rename listadonominal listanominal

destring listanominal pan - total , replace

collapse (sum) listanominal pan - total , by (municipality section)
   
rename pan   PAN
rename pri   PRI
rename pri_pvem_panal   PRI_PVEM_PANAL
rename prd   PRD
rename pt    PT
rename pvem  PVEM
rename pc    PC
rename panal PANAL

gen turnout =  total/listanominal

drop    nulos noregistrado

gen   uniqueid= 0
replace uniqueid=28001 if municipality =="Abasolo"
replace uniqueid=28002 if municipality =="Aldama"
replace uniqueid=28003 if municipality =="Altamira"
replace uniqueid=28004 if municipality =="Antiguo Morelos"
replace uniqueid=28005 if municipality =="Burgos"
replace uniqueid=28006 if municipality =="Bustamante"
replace uniqueid=28007 if municipality =="Camargo"
replace uniqueid=28008 if municipality =="Casas"
replace uniqueid=28009 if municipality =="Ciudad Madero"
replace uniqueid=28010 if municipality =="Cruillas"
replace uniqueid=28021 if municipality =="El Mante"
replace uniqueid=28011 if municipality =="Gómez Farías"
replace uniqueid=28012 if municipality =="González"
replace uniqueid=28013 if municipality =="Güémez"
replace uniqueid=28014 if municipality =="Guerrero"
replace uniqueid=28015 if municipality =="Gustavo Díaz Ordaz"
replace uniqueid=28016 if municipality =="Hidalgo"
replace uniqueid=28017 if municipality =="Jaumave"
replace uniqueid=28018 if municipality =="Jiménez"
replace uniqueid=28019 if municipality =="Llera"
replace uniqueid=28020 if municipality =="Mainero"
replace uniqueid=28022 if municipality =="Matamoros"
replace uniqueid=28023 if municipality =="Méndez"
replace uniqueid=28024 if municipality =="Mier"
replace uniqueid=28025 if municipality =="Miguel Alemán"
replace uniqueid=28026 if municipality =="Miquihuana"
replace uniqueid=28027 if municipality =="Nuevo Laredo"
replace uniqueid=28028 if municipality =="Nuevo Morelos"
replace uniqueid=28029 if municipality =="Ocampo"
replace uniqueid=28030 if municipality =="Padilla"
replace uniqueid=28031 if municipality =="Palmillas"
replace uniqueid=28032 if municipality =="Reynosa"
replace uniqueid=28033 if municipality =="Río Bravo"
replace uniqueid=28034 if municipality =="San Carlos"
replace uniqueid=28035 if municipality =="San Fernando"
replace uniqueid=28036 if municipality =="San Nicolás"
replace uniqueid=28037 if municipality =="Soto la Marina"
replace uniqueid=28038 if municipality =="Tampico"
replace uniqueid=28039 if municipality =="Tula"
replace uniqueid=28040 if municipality =="Valle Hermoso"
replace uniqueid=28041 if municipality =="Victoria"
replace uniqueid=28042 if municipality =="Villagrán"
replace uniqueid=28043 if municipality =="Xicoténcatl"

egen valid = rowtotal(PAN PRI PRI_PVEM_PANAL PRD PT PVEM PANAL PC)

foreach var in PAN PRI PRI_PVEM_PANAL PRD PT PVEM PANAL PC listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRI_PVEM_PANAL inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PANAL inv_mun_PC, gen(PAN_r PRI_r PRI_PVEM_PANAL_r PRD_r PT_r PVEM_r PANAL_r PC_r)
drop inv_mun_*

gen winner = "PAN" if PAN_r==1  
replace winner = "PRI" if PRI_r ==1 
replace winner = "PRI_PVEM_PANAL" if PRI_PVEM_PANAL_r ==1 
replace winner = "PRD" if PRD_r ==1 
replace winner = "PT" if PT_r ==1 
replace winner = "PVEM" if PVEM_r ==1 
replace winner = "PANAL" if PANAL_r ==1 
replace winner = "PC" if PC_r ==1 
drop *_r

gen year = 2013
gen month ="July"

sort section

save Tamaulipas_Section_2013.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************


clear all
set mem 1g

use Tamaulipas_Section_1995.dta
append using Tamaulipas_Section_1998.dta
append using Tamaulipas_Section_2001.dta
append using Tamaulipas_Section_2004.dta
append using Tamaulipas_Section_2007.dta
append using Tamaulipas_Section_2010.dta
append using Tamaulipas_Section_2013.dta

erase Tamaulipas_Section_1995.dta
erase Tamaulipas_Section_1998.dta
erase Tamaulipas_Section_2001.dta
erase Tamaulipas_Section_2004.dta
erase Tamaulipas_Section_2007.dta
erase Tamaulipas_Section_2010.dta
erase Tamaulipas_Section_2013.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoCardenista CDPPN PUP PEC PFC {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Tamaulipas_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"

save Tamaulipas_ALL.dta, replace
