
clear all
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/San Luis Potosi - 1997, 2000, 2003, 2006, 2009, 2012"

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_1997_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total ,replace

collapse (sum)  pan - total , by (municipality section)

rename pan  PAN
rename pri  PRI
rename prd  PRD
rename pt   PT
rename pvem  PVEM
rename pfcrn PartidoCardenista
rename pps PPS
* Partido Demócrata Mexicano
rename pdm PDM
* Nava Partido Político
rename npp NPP

* gen turnout =  total/listanominal

drop    nulos noreg  

gen   uniqueid= 0
replace uniqueid=24001 if municipality =="AHUALULCO"
replace uniqueid=24002 if municipality =="ALAQUINES"
replace uniqueid=24003 if municipality =="AQUISMON"
replace uniqueid=24004 if municipality =="ARMADILLO DE LOS INFANTE"
replace uniqueid=24053 if municipality =="AXTLA DE TERRAZAS"
replace uniqueid=24005 if municipality =="CARDENAS"
replace uniqueid=24006 if municipality =="CATORCE"
replace uniqueid=24007 if municipality =="CEDRAL"
replace uniqueid=24008 if municipality =="CERRITOS"
replace uniqueid=24009 if municipality =="CERRO DE SAN PEDRO"
replace uniqueid=24015 if municipality =="CHARCAS"
replace uniqueid=24010 if municipality =="CIUDAD DEL MAIZ"
replace uniqueid=24011 if municipality =="CIUDAD FERNANDEZ"
replace uniqueid=24013 if municipality =="CIUDAD VALLES"
replace uniqueid=24014 if municipality =="COXCATLAN"
replace uniqueid=24016 if municipality =="EBANO"
replace uniqueid=24058 if municipality =="EL NARANJO"
replace uniqueid=24017 if municipality =="GUADALCAZAR"
replace uniqueid=24018 if municipality =="HUEHUETLAN"
replace uniqueid=24019 if municipality =="LAGUNILLAS"
replace uniqueid=24020 if municipality =="MATEHUALA"
replace uniqueid=24057 if municipality =="MATLAPA"
replace uniqueid=24021 if municipality =="MEXQUITIC DE CARMONA"
replace uniqueid=24022 if municipality =="MOCTEZUMA"
replace uniqueid=24023 if municipality =="RAYON"
replace uniqueid=24024 if municipality =="RIOVERDE"
replace uniqueid=24025 if municipality =="SALINAS"
replace uniqueid=24026 if municipality =="SAN ANTONIO"
replace uniqueid=24027 if municipality =="SAN CIRO DE ACOSTA"
replace uniqueid=24028 if municipality =="SAN LUIS POTOSI"
replace uniqueid=24029 if municipality =="SAN MARTIN CHALCHICUAUTLA"
replace uniqueid=24030 if municipality =="SAN NICOLAS TOLENTINO"
replace uniqueid=24034 if municipality =="SAN VICENTE TANCUAYALAB"
replace uniqueid=24031 if municipality =="SANTA CATARINA"
replace uniqueid=24032 if municipality =="SANTA MARIA DEL RIO"
replace uniqueid=24033 if municipality =="SANTO DOMINGO"
replace uniqueid=24035 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace uniqueid=24036 if municipality =="TAMASOPO"
replace uniqueid=24037 if municipality =="TAMAZUNCHALE"
replace uniqueid=24038 if municipality =="TAMPACAN"
replace uniqueid=24039 if municipality =="TAMPAMOLON CORONA"
replace uniqueid=24040 if municipality =="TAMUIN"
replace uniqueid=24012 if municipality =="TANCANHUITZ DE SANTOS"
replace uniqueid=24041 if municipality =="TANLAJAS"
replace uniqueid=24042 if municipality =="TANQUIAN DE ESCOBEDO"
replace uniqueid=24043 if municipality =="TIERRA NUEVA"
replace uniqueid=24044 if municipality =="VANEGAS"
replace uniqueid=24045 if municipality =="VENADO"
replace uniqueid=24056 if municipality =="VILLA DE ARISTA"
replace uniqueid=24046 if municipality =="VILLA DE ARRIAGA"
replace uniqueid=24047 if municipality =="VILLA DE GUADALUPE"
replace uniqueid=24048 if municipality =="VILLA DE LA PAZ"
replace uniqueid=24049 if municipality =="VILLA DE RAMOS"
replace uniqueid=24050 if municipality =="VILLA DE REYES"
replace uniqueid=24051 if municipality =="VILLA HIDALGO"
replace uniqueid=24052 if municipality =="VILLA JUAREZ"
replace uniqueid=24054 if municipality =="XILITLA"
replace uniqueid=24055 if municipality =="ZARAGOZA"


egen valid = rowtotal(PAN PRI PRD PartidoCardenista PT PVEM PPS PDM NPP)

foreach var in PAN PRI PRD PartidoCardenista PT PVEM PPS PDM NPP total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PartidoCardenista inv_mun_PT inv_mun_PVEM inv_mun_PPS inv_mun_PDM inv_mun_NPP, gen(PAN_r PRI_r PRD_r PartidoCardenista_r PT_r PVEM_r PPS_r PDM_r NPP_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PartidoCardenista PT PVEM PPS PDM NPP {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r


gen year = 1997
gen month ="July"

save San_Luis_Potosi_Section_1997.dta,replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2000.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  pan - listanominal ,replace

collapse (sum)   pan - listanominal , by (municipality section)

rename pan   PAN
rename pri   PRI
rename prd   PRD
rename pt    PT
rename pvem  PVEM
rename pcd   PCD
rename parm  PARM
rename pas   PAS
rename pds   PDS
* Nava Partido Político
rename npp   NPP
rename psn   PSN
* Partido Conciencia Popular
rename pcp   PCP
rename axslp PRD_PT_PCD_PSN
rename cfcp  PVEM_PCP

replace PRD_PT_PCD_PSN = PRD_PT_PCD_PSN + PCD if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace PCD = 0 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"

gen turnout = total / listanominal

drop    nulos noreg   

gen   uniqueid= 0
replace uniqueid=24001 if municipality =="AHUALULCO"
replace uniqueid=24002 if municipality =="ALAQUINES"
replace uniqueid=24003 if municipality =="AQUISMON"
replace uniqueid=24004 if municipality =="ARMADILLO DE LOS INFANTE"
replace uniqueid=24053 if municipality =="AXTLA DE TERRAZAS"
replace uniqueid=24005 if municipality =="CARDENAS"
replace uniqueid=24006 if municipality =="CATORCE"
replace uniqueid=24007 if municipality =="CEDRAL"
replace uniqueid=24008 if municipality =="CERRITOS"
replace uniqueid=24009 if municipality =="CERRO DE SAN PEDRO"
replace uniqueid=24015 if municipality =="CHARCAS"
replace uniqueid=24010 if municipality =="CIUDAD DEL MAIZ"
replace uniqueid=24011 if municipality =="CIUDAD FERNANDEZ"
replace uniqueid=24013 if municipality =="CIUDAD VALLES"
replace uniqueid=24014 if municipality =="COXCATLAN"
replace uniqueid=24016 if municipality =="EBANO"
replace uniqueid=24058 if municipality =="EL NARANJO"
replace uniqueid=24017 if municipality =="GUADALCAZAR"
replace uniqueid=24018 if municipality =="HUEHUETLAN"
replace uniqueid=24019 if municipality =="LAGUNILLAS"
replace uniqueid=24020 if municipality =="MATEHUALA"
replace uniqueid=24057 if municipality =="MATLAPA"
replace uniqueid=24021 if municipality =="MEXQUITIC DE CARMONA"
replace uniqueid=24022 if municipality =="MOCTEZUMA"
replace uniqueid=24023 if municipality =="RAYON"
replace uniqueid=24024 if municipality =="RIOVERDE"
replace uniqueid=24025 if municipality =="SALINAS"
replace uniqueid=24026 if municipality =="SAN ANTONIO"
replace uniqueid=24027 if municipality =="SAN CIRO DE ACOSTA"
replace uniqueid=24028 if municipality =="SAN LUIS POTOSI"
replace uniqueid=24029 if municipality =="SAN MARTIN CHALCHICUAHUTLA"
replace uniqueid=24030 if municipality =="SAN NICOLAS TOLENTINO"
replace uniqueid=24034 if municipality =="SAN VICENTE TANCUAYALAB"
replace uniqueid=24031 if municipality =="SANTA CATARINA"
replace uniqueid=24032 if municipality =="SANTA MARIA DEL RIO"
replace uniqueid=24033 if municipality =="SANTO DOMINGO"
replace uniqueid=24035 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace uniqueid=24036 if municipality =="TAMASOPO"
replace uniqueid=24037 if municipality =="TAMAZUNCHALE"
replace uniqueid=24038 if municipality =="TAMPACAN"
replace uniqueid=24039 if municipality =="TAMPAMOLON CORONA"
replace uniqueid=24040 if municipality =="TAMUIN"
replace uniqueid=24012 if municipality =="TANCANHUITZ DE SANTOS"
replace uniqueid=24041 if municipality =="TANLAJAS"
replace uniqueid=24042 if municipality =="TANQUIAN DE ESCOBEDO"
replace uniqueid=24043 if municipality =="TIERRA NUEVA"
replace uniqueid=24044 if municipality =="VANEGAS"
replace uniqueid=24045 if municipality =="VENADO"
replace uniqueid=24056 if municipality =="VILLA DE ARISTA"
replace uniqueid=24046 if municipality =="VILLA DE ARRIAGA"
replace uniqueid=24047 if municipality =="VILLA DE GUADALUPE"
replace uniqueid=24048 if municipality =="VILLA DE LA PAZ"
replace uniqueid=24049 if municipality =="VILLA DE RAMOS"
replace uniqueid=24050 if municipality =="VILLA DE REYES"
replace uniqueid=24051 if municipality =="VILLA HIDALGO"
replace uniqueid=24052 if municipality =="VILLA JUAREZ"
replace uniqueid=24054 if municipality =="XILITLA"
replace uniqueid=24055 if municipality =="ZARAGOZA"


egen valid = rowtotal(PAN PRI PRD NPP PT PVEM PCP PSN PVEM_PCP PRD_PT_PCD_PSN PCD PARM PAS PDS)

foreach var in PAN PRI PRD NPP PT PVEM PCP PSN PVEM_PCP PRD_PT_PCD_PSN PCD PARM PAS PDS  total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_NPP inv_mun_PT inv_mun_PVEM inv_mun_PCP inv_mun_PSN inv_mun_PVEM_PCP inv_mun_PRD_PT_PCD_PSN inv_mun_PCD inv_mun_PARM inv_mun_PAS inv_mun_PDS, gen(PAN_r PRI_r PRD_r NPP_r PT_r PVEM_r PCP_r PSN_r PVEM_PCP_r PRD_PT_PCD_PSN_r PCD_r PARM_r PAS_r PDS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD NPP PT PVEM PCP PSN PVEM_PCP PRD_PT_PCD_PSN PCD PARM PAS PDS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2000
gen month ="July"

save San_Luis_Potosi_Section_2000.dta,replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2003.csv, clear

rename municipio  municipality
rename seccion section

drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal - total ,replace

collapse (sum)   listanominal - total , by (municipality section  coalicion1 coalicion2)
 
******************************************************************************************************

gen PRI_PVEM_PCP = 0 
replace PRI_PVEM_PCP = pcp + pripvem + cc1 if  coalicion1 =="PCP-APT"
replace pcp = 0 if  coalicion1 =="PCP-APT"
replace pripvem = 0 if  coalicion1 =="PCP-APT"
replace cc1 = 0 if  coalicion1 =="PCP-APT"
replace sumacc1 = 0 if  coalicion1 =="PCP-APT" & PRI_PVEM_PCP==sumacc1

gen PCP_Convergencia = 0 
replace PCP_Convergencia = pcp + pc + cc1 if  coalicion1 =="PCP-Convergencia"
replace pcp = 0 if  coalicion1 =="PCP-Convergencia"
replace pc  = 0 if  coalicion1 =="PCP-Convergencia"
replace cc1 = 0 if  coalicion1 =="PCP-Convergencia"
replace sumacc1 = 0 if  coalicion1 =="PCP-Convergencia" & PCP_Convergencia==sumacc1

gen PCP_PAS_Convergencia =0
replace PCP_PAS_Convergencia = pcp + pas + pc + cc1 if  coalicion1 =="PCP-PAS-Convergencia"
replace pcp = 0 if  coalicion1 =="PCP-PAS-Convergencia"
replace pas = 0 if  coalicion1 =="PCP-PAS-Convergencia"
replace pc  = 0 if  coalicion1 =="PCP-PAS-Convergencia"
replace cc1 = 0 if  coalicion1 =="PCP-PAS-Convergencia"
replace sumacc1 = 0  if  coalicion1 =="PCP-PAS-Convergencia" & PCP_PAS_Convergencia==sumacc1

gen PRD_PT = 0
replace PRD_PT = prd + pt + cc1 if  coalicion1 =="PRD-PT"
replace prd = 0  if  coalicion1 =="PRD-PT"
replace pt = 0  if  coalicion1 =="PRD-PT"
replace cc1 = 0  if  coalicion1 =="PRD-PT"
replace sumacc1 = 0   if  coalicion1 =="PRD-PT" & PRD_PT==sumacc1

gen PRD_PT_Convergencia = 0
replace PRD_PT_Convergencia = prd + pt + pc + cc1 if  coalicion1 =="PRD-PT-Convergencia"
replace prd  = 0  if  coalicion1 =="PRD-PT-Convergencia"
replace pt  = 0  if  coalicion1 =="PRD-PT-Convergencia"
replace pc = 0  if  coalicion1 =="PRD-PT-Convergencia"
replace cc1 = 0  if  coalicion1 =="PRD-PT-Convergencia"
replace sumacc1 = 0   if  coalicion1 =="PRD-PT-Convergencia" & PRD_PT_Convergencia==sumacc1

gen PRD_PT_PCP_Convergencia = 0
replace PRD_PT_PCP_Convergencia = prd + pt + pcp+ pc + cc1 if coalicion1 =="PRD-PT-PCP-Convergencia"
replace prd = 0 if coalicion1 =="PRD-PT-PCP-Convergencia"
replace pt = 0 if coalicion1 =="PRD-PT-PCP-Convergencia"
replace pcp = 0 if coalicion1 =="PRD-PT-PCP-Convergencia"
replace pc = 0 if coalicion1 =="PRD-PT-PCP-Convergencia"
replace cc1 = 0 if coalicion1 =="PRD-PT-PCP-Convergencia" 
replace sumacc1 = 0  if coalicion1 =="PRD-PT-PCP-Convergencia" &  PRD_PT_PCP_Convergencia== sumacc1

gen PRD_PVEM_Convergencia = 0
replace PRD_PVEM_Convergencia = prd + pvem + pc + cc1 if  coalicion1 =="PRD-PVEM-Convergencia"
replace prd = 0  if  coalicion1 =="PRD-PVEM-Convergencia"
replace pvem = 0  if  coalicion1 =="PRD-PVEM-Convergencia"
replace pc = 0  if  coalicion1 =="PRD-PVEM-Convergencia"
replace cc1 = 0  if  coalicion1 =="PRD-PVEM-Convergencia"
replace sumacc1 = 0  if  coalicion1 =="PRD-PVEM-Convergencia" & PRD_PVEM_Convergencia==sumacc1

gen PRI_PCP = 0
replace PRI_PCP = pri + pcp + cc1 if coalicion1 =="PRI-PCP"
replace pri = 0  if coalicion1 =="PRI-PCP"
replace pcp = 0  if coalicion1 =="PRI-PCP"
replace cc1 = 0  if coalicion1 =="PRI-PCP"
replace sumacc1 = 0   if coalicion1 =="PRI-PCP" & PRI_PCP==sumacc1

gen PRI_PT = 0
replace PRI_PT = pri + pt + cc1 if coalicion1 =="PRI-PT"
replace pri = 0  if coalicion1 =="PRI-PT"
replace pt = 0  if coalicion1 =="PRI-PT"
replace cc1 = 0  if coalicion1 =="PRI-PT"
replace sumacc1 = 0  if coalicion1 =="PRI-PT" &  PRI_PT==sumacc1


gen PRI_PT_PVEM_PAS = 0
replace PRI_PT_PVEM_PAS = pri + pt + pvem + pas + cc1 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace pri = 0 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace pt = 0 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace pvem = 0 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace pas = 0 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace cc1 = 0 if  coalicion1 =="PRI-PT-PVEM-PAS"
replace sumacc1 = 0   if  coalicion1 =="PRI-PT-PVEM-PAS" &  PRI_PT_PVEM_PAS==sumacc1

replace pripvem = 0
replace pripvem = pri + pvem + cc1 if  coalicion1 =="PRI-PVEM"
replace pri = 0  if  coalicion1 =="PRI-PVEM"
replace pvem = 0  if  coalicion1 =="PRI-PVEM"
replace cc1 = 0  if  coalicion1 =="PRI-PVEM"
replace sumacc1 = 0   if  coalicion1 =="PRI-PVEM" & PRI_PVEM==sumacc1

gen PRI_PT_PVEM = 0
replace PRI_PT_PVEM = pt + pripvem + cc1 if  coalicion1 =="PT-APT"
replace pt = 0  if  coalicion1 =="PT-APT"
replace pripvem = 0  if  coalicion1 =="PT-APT"
replace cc1 = 0  if  coalicion1 =="PT-APT"
replace sumacc1 = 0   if  coalicion1 =="PT-APT" & PRI_PT_PVEM ==sumacc1

gen PT_Convergencia = 0
replace PT_Convergencia = pt + pc + cc1 if  coalicion1 =="PT-Convergencia"
replace pt = 0  if  coalicion1 =="PT-Convergencia"
replace pc = 0  if  coalicion1 =="PT-Convergencia"
replace cc1 = 0  if  coalicion1 =="PT-Convergencia"
replace sumacc1 = 0   if  coalicion1 =="PT-Convergencia" & PT_Convergencia==sumacc1

gen PT_PCP = 0
replace PT_PCP = pt + pcp + cc1 if  coalicion1 =="PT-PCP"
replace pt = 0  if  coalicion1 =="PT-PCP"
replace pcp = 0  if  coalicion1 =="PT-PCP"
replace cc1 = 0  if  coalicion1 =="PT-PCP"
replace sumacc1 = 0   if  coalicion1 =="PT-PCP" & PT_PCP==sumacc1


gen PT_PCP_Convergencia = 0
replace PT_PCP_Convergencia = pt + pcp + pc + cc1 if coalicion1 =="PT-PCP-Convergencia"
replace pt = 0 if coalicion1 =="PT-PCP-Convergencia"
replace pcp = 0 if coalicion1 =="PT-PCP-Convergencia"
replace pc = 0 if coalicion1 =="PT-PCP-Convergencia"
replace cc1 = 0 if coalicion1 =="PT-PCP-Convergencia"
replace sumacc1 = 0  if coalicion1 =="PT-PCP-Convergencia" & PT_PCP_Convergencia==sumacc1

sum cc1 sumacc1
drop  cc1 sumacc1

replace PRD_PT = prd + pt + cc2 if  coalicion2 =="PRD-PT"
replace prd = 0 if  coalicion2 =="PRD-PT"
replace pt = 0 if  coalicion2 =="PRD-PT"
replace cc2 = 0 if  coalicion2 =="PRD-PT"
replace sumacc2 = 0  if  coalicion2 =="PRD-PT" &  sumacc2 == PRD_PT

replace PRD_PT_Convergencia = prd + pt + pc + cc2 if  coalicion2 =="PRD-PT-Convergencia"
replace prd = 0 if  coalicion2 =="PRD-PT-Convergencia"
replace pt = 0 if  coalicion2 =="PRD-PT-Convergencia"
replace pc = 0 if  coalicion2 =="PRD-PT-Convergencia"
replace cc2 = 0 if  coalicion2 =="PRD-PT-Convergencia"
replace sumacc2 = 0  if  coalicion2 =="PRD-PT-Convergencia" &  sumacc2 == PRD_PT_Convergencia

gen PRD_PT_PSN_Convergencia =0 
replace PRD_PT_PSN_Convergencia = prd + pt + psn + pc + cc2 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace prd = 0 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace pt = 0 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace psn = 0 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace pc = 0 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace cc2 = 0 if  coalicion2 =="PRD-PT-PSN-Convergencia"
replace sumacc2 = 0  if  coalicion2 =="PRD-PT-PSN-Convergencia" &  sumacc2 == PRD_PT_PSN_Convergencia

sum cc2 sumacc2
drop cc2 sumacc2
drop coalicion1 coalicion2
 
******************************************************************************************************
            
rename pan PAN
rename pri PRI
rename pripvem PRI_PVEM
rename prd PRD
rename PRD_PT_Convergencia PRD_PT_PC
rename PRD_PT_PCP_Convergencia PRD_PT_PCP_PC
rename PRD_PVEM_Convergencia PRD_PVEM_PC
rename PRD_PT_PSN_Convergencia PRD_PT_PSN_PC
rename pt  PT
rename PT_Convergencia PT_PC
rename PT_PCP_Convergencia PT_PCP_PC
rename pvem  PVEM
rename pcp PCP
rename PCP_Convergencia PCP_PC
rename PCP_PAS_Convergencia  PCP_PAS_PC
rename pas PAS
rename psn PSN
rename pc PC

gen turnout =  total/listanominal

drop    nulos noreg 

gen   uniqueid= 0
replace uniqueid=24001 if municipality =="AHUALULCO"
replace uniqueid=24002 if municipality =="ALAQUINES"
replace uniqueid=24003 if municipality =="AQUISMON"
replace uniqueid=24004 if municipality =="ARMADILLO DE LOS INFANTE"
replace uniqueid=24053 if municipality =="AXTLA DE TERRAZAS"
replace uniqueid=24005 if municipality =="CARDENAS"
replace uniqueid=24006 if municipality =="CATORCE"
replace uniqueid=24007 if municipality =="CEDRAL"
replace uniqueid=24008 if municipality =="CERRITOS"
replace uniqueid=24009 if municipality =="CERRO DE SAN PEDRO"
replace uniqueid=24015 if municipality =="CHARCAS"
replace uniqueid=24010 if municipality =="CIUDAD DEL MAIZ"
replace uniqueid=24011 if municipality =="CIUDAD FERNANDEZ"
replace uniqueid=24013 if municipality =="CIUDAD VALLES"
replace uniqueid=24014 if municipality =="COXCATLAN"
replace uniqueid=24016 if municipality =="EBANO"
replace uniqueid=24058 if municipality =="EL NARANJO"
replace uniqueid=24017 if municipality =="GUADALCAZAR"
replace uniqueid=24018 if municipality =="HUEHUETLAN"
replace uniqueid=24019 if municipality =="LAGUNILLAS"
replace uniqueid=24020 if municipality =="MATEHUALA"
replace uniqueid=24057 if municipality =="MATLAPA"
replace uniqueid=24021 if municipality =="MEXQUITIC DE CARMONA"
replace uniqueid=24022 if municipality =="MOCTEZUMA"
replace uniqueid=24023 if municipality =="RAYON"
replace uniqueid=24024 if municipality =="RIOVERDE"
replace uniqueid=24025 if municipality =="SALINAS"
replace uniqueid=24026 if municipality =="SAN ANTONIO"
replace uniqueid=24027 if municipality =="SAN CIRO DE ACOSTA"
replace uniqueid=24028 if municipality =="SAN LUIS POTOSI"
replace uniqueid=24029 if municipality =="SAN MARTIN CHALCHICUAUTLA"
replace uniqueid=24030 if municipality =="SAN NICOLAS TOLENTINO"
replace uniqueid=24034 if municipality =="SAN VICENTE TANCUAYALAB"
replace uniqueid=24031 if municipality =="SANTA CATARINA"
replace uniqueid=24032 if municipality =="SANTA MARIA DEL RIO"
replace uniqueid=24033 if municipality =="SANTO DOMINGO"
replace uniqueid=24035 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace uniqueid=24036 if municipality =="TAMASOPO"
replace uniqueid=24037 if municipality =="TAMAZUNCHALE"
replace uniqueid=24038 if municipality =="TAMPACAN"
replace uniqueid=24039 if municipality =="TAMPAMOLON CORONA"
replace uniqueid=24040 if municipality =="TAMUIN"
replace uniqueid=24012 if municipality =="TANCANHUITZ"
replace uniqueid=24041 if municipality =="TANLAJAS"
replace uniqueid=24042 if municipality =="TANQUIAN DE ESCOBEDO"
replace uniqueid=24043 if municipality =="TIERRANUEVA"
replace uniqueid=24044 if municipality =="VANEGAS"
replace uniqueid=24045 if municipality =="VENADO"
replace uniqueid=24056 if municipality =="VILLA DE ARISTA"
replace uniqueid=24046 if municipality =="VILLA DE ARRIAGA"
replace uniqueid=24047 if municipality =="VILLA DE GUADALUPE"
replace uniqueid=24048 if municipality =="VILLA DE LA PAZ"
replace uniqueid=24049 if municipality =="VILLA DE RAMOS"
replace uniqueid=24050 if municipality =="VILLA DE REYES"
replace uniqueid=24051 if municipality =="VILLA HIDALGO"
replace uniqueid=24052 if municipality =="VILLA JUAREZ"
replace uniqueid=24054 if municipality =="XILITLA"
replace uniqueid=24055 if municipality =="ZARAGOZA"

egen valid = rowtotal(PAN PRI PRD PT PVEM PCP PAS PSN PC PRI_PVEM PRI_PVEM_PCP PCP_PC PCP_PAS_PC PRD_PT PRD_PT_PC PRD_PT_PCP_PC PRD_PVEM_PC PRI_PCP PRI_PT PRI_PT_PVEM_PAS PRI_PT_PVEM PT_PC PT_PCP PT_PCP_PC PRD_PT_PSN_PC)

foreach var in PAN PRI PRD PT PVEM PCP PAS PSN PC PRI_PVEM PRI_PVEM_PCP PCP_PC PCP_PAS_PC PRD_PT PRD_PT_PC PRD_PT_PCP_PC PRD_PVEM_PC PRI_PCP PRI_PT PRI_PT_PVEM_PAS PRI_PT_PVEM PT_PC PT_PCP PT_PCP_PC PRD_PT_PSN_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PCP inv_mun_PAS inv_mun_PSN inv_mun_PC inv_mun_PRI_PVEM inv_mun_PRI_PVEM_PCP inv_mun_PCP_PC inv_mun_PCP_PAS_PC inv_mun_PRD_PT inv_mun_PRD_PT_PC inv_mun_PRD_PT_PCP_PC inv_mun_PRD_PVEM_PC inv_mun_PRI_PCP inv_mun_PRI_PT inv_mun_PRI_PT_PVEM_PAS inv_mun_PRI_PT_PVEM inv_mun_PT_PC inv_mun_PT_PCP inv_mun_PT_PCP_PC inv_mun_PRD_PT_PSN_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PCP_r PAS_r PSN_r PC_r PRI_PVEM_r PRI_PVEM_PCP_r PCP_PC_r PCP_PAS_PC_r PRD_PT_r PRD_PT_PC_r PRD_PT_PCP_PC_r PRD_PVEM_PC_r PRI_PCP_r PRI_PT_r PRI_PT_PVEM_PAS_r PRI_PT_PVEM_r PT_PC_r PT_PCP_r PT_PCP_PC_r PRD_PT_PSN_PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PCP PAS PSN PC PRI_PVEM PRI_PVEM_PCP PCP_PC PCP_PAS_PC PRD_PT PRD_PT_PC PRD_PT_PCP_PC PRD_PVEM_PC PRI_PCP PRI_PT PRI_PT_PVEM_PAS PRI_PT_PVEM PT_PC PT_PCP PT_PCP_PC PRD_PT_PSN_PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2003
gen month ="October"

sort section

save San_Luis_Potosi_Section_2003.dta,replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring   listanominal nulos - total ,replace

collapse (sum)   listanominal nulos - total , by (municipality section)

**************************************************************************

foreach var in  panpna panpcppna pripasc pripcppasc pvempcp pvempcppna pcppna  {
bys municipality: egen c_`var' = sum(`var')
replace c_`var' = (c_`var'>0)
}


replace panpna = panpna + pan + pna if c_panpna==1
replace pan =0 if c_panpna==1
replace pna =0 if c_panpna==1

replace panpcppna = panpcppna + pan + pcp + pna if c_panpcppna==1
replace pan  =0  if c_panpcppna==1
replace pcp  =0  if c_panpcppna==1
replace pna  =0 if c_panpcppna==1

replace pvempcp = pvempcp  + pvem  + pcp 
replace pvem =0  if c_pvempcp==1
replace pcp  =0  if c_pvempcp==1 

replace pvempcppna = pvempcppna + pvem  + pcp  + pna if c_pvempcppna==1
replace pvem=0  if c_pvempcppna==1
replace pcp =0  if c_pvempcppna==1
replace pna =0  if c_pvempcppna==1

replace pcppna = pcppna  + pcp  + pna if c_pcppna==1
replace pcp  = 0   if c_pcppna==1
replace pna  = 0  if c_pcppna==1

replace pripasc = pripasc + pri + pasc if c_pripasc==1
replace pri = 0 if c_pripasc==1
replace pasc = 0 if c_pripasc==1

replace pripcppasc = pripcppasc + pri + pcp + pasc if c_pripcppasc==1
replace pri  = 0 if c_pripcppasc==1
replace pcp  = 0 if c_pripcppasc==1
replace pasc = 0 if c_pripcppasc==1

**************************************************************************

rename pan PAN
rename pri PRI
rename prdptconvergencia PRD_PT_PC
rename pvem  PVEM
rename pcp PCP
rename pna PANAL
rename pasc PASC
rename panpna PAN_PANAL
rename panpcppna PAN_PCP_PANAL
rename pvempcp PVEM_PCP
rename pvempcppna PVEM_PCP_PANAL
rename pcppna PCP_PANAL
rename pripasc PRI_PASC
rename pripcppasc PRI_PCP_PASC

gen turnout =  total/listanominal

drop  nulos noreg  

gen   uniqueid= 0
replace uniqueid=24001 if municipality =="AHUALULCO"
replace uniqueid=24002 if municipality =="ALAQUINES"
replace uniqueid=24003 if municipality =="AQUISMON"
replace uniqueid=24004 if municipality =="ARMADILLO DE LOS INFANTE"
replace uniqueid=24053 if municipality =="AXTLA DE TERRAZAS"
replace uniqueid=24005 if municipality =="CARDENAS"
replace uniqueid=24006 if municipality =="CATORCE"
replace uniqueid=24007 if municipality =="CEDRAL"
replace uniqueid=24008 if municipality =="CERRITOS"
replace uniqueid=24009 if municipality =="CERRO DE SAN PEDRO"
replace uniqueid=24015 if municipality =="CHARCAS"
replace uniqueid=24010 if municipality =="CIUDAD DEL MAIZ"
replace uniqueid=24011 if municipality =="CIUDAD FERNANDEZ"
replace uniqueid=24013 if municipality =="CIUDAD VALLES"
replace uniqueid=24014 if municipality =="COXCATLAN"
replace uniqueid=24016 if municipality =="EBANO"
replace uniqueid=24058 if municipality =="EL NARANJO"
replace uniqueid=24017 if municipality =="GUADALCAZAR"
replace uniqueid=24018 if municipality =="HUEHUETLAN"
replace uniqueid=24019 if municipality =="LAGUNILLAS"
replace uniqueid=24020 if municipality =="MATEHUALA"
replace uniqueid=24057 if municipality =="MATLAPA"
replace uniqueid=24021 if municipality =="MEXQUITIC DE CARMONA"
replace uniqueid=24022 if municipality =="MOCTEZUMA"
replace uniqueid=24023 if municipality =="RAYON"
replace uniqueid=24024 if municipality =="RIOVERDE"
replace uniqueid=24025 if municipality =="SALINAS"
replace uniqueid=24026 if municipality =="SAN ANTONIO"
replace uniqueid=24027 if municipality =="SAN CIRO DE ACOSTA"
replace uniqueid=24028 if municipality =="SAN LUIS POTOSI"
replace uniqueid=24029 if municipality =="SAN MARTIN CHALCHICUAUTLA"
replace uniqueid=24030 if municipality =="SAN NICOLAS TOLENTINO"
replace uniqueid=24034 if municipality =="SAN VICENTE TANCUAYALAB"
replace uniqueid=24031 if municipality =="SANTA CATARINA"
replace uniqueid=24032 if municipality =="SANTA MARIA DEL RIO"
replace uniqueid=24033 if municipality =="SANTO DOMINGO"
replace uniqueid=24035 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace uniqueid=24036 if municipality =="TAMASOPO"
replace uniqueid=24037 if municipality =="TAMAZUNCHALE"
replace uniqueid=24038 if municipality =="TAMPACAN"
replace uniqueid=24039 if municipality =="TAMPAMOLON CORONA"
replace uniqueid=24040 if municipality =="TAMUIN"
replace uniqueid=24012 if municipality =="TANCANHUITZ"
replace uniqueid=24041 if municipality =="TANLAJAS"
replace uniqueid=24042 if municipality =="TANQUIAN DE ESCOBEDO"
replace uniqueid=24043 if municipality =="TIERRA NUEVA"
replace uniqueid=24044 if municipality =="VANEGAS"
replace uniqueid=24045 if municipality =="VENADO"
replace uniqueid=24056 if municipality =="VILLA DE ARISTA"
replace uniqueid=24046 if municipality =="VILLA DE ARRIAGA"
replace uniqueid=24047 if municipality =="VILLA DE GUADALUPE"
replace uniqueid=24048 if municipality =="VILLA DE LA PAZ"
replace uniqueid=24049 if municipality =="VILLA DE RAMOS"
replace uniqueid=24050 if municipality =="VILLA DE REYES"
replace uniqueid=24051 if municipality =="VILLA HIDALGO"
replace uniqueid=24052 if municipality =="VILLA JUAREZ"
replace uniqueid=24054 if municipality =="XILITLA"
replace uniqueid=24055 if municipality =="ZARAGOZA"

egen valid = rowtotal(PAN PRI PRD_PT_PC PVEM PCP PANAL PASC PAN_PANAL PAN_PCP_PANAL PRI_PASC PRI_PCP_PASC PVEM_PCP PVEM_PCP_PANAL PCP_PANAL)

foreach var in PAN PRI PRD_PT_PC PVEM PCP PANAL PASC PAN_PANAL PAN_PCP_PANAL PRI_PASC PRI_PCP_PASC PVEM_PCP PVEM_PCP_PANAL PCP_PANAL total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT_PC inv_mun_PVEM inv_mun_PCP inv_mun_PANAL inv_mun_PASC inv_mun_PAN_PANAL inv_mun_PAN_PCP_PANAL inv_mun_PRI_PASC inv_mun_PRI_PCP_PASC inv_mun_PVEM_PCP inv_mun_PVEM_PCP_PANAL inv_mun_PCP_PANAL, gen(PAN_r PRI_r PRD_PT_PC_r PVEM_r PCP_r PANAL_r PASC_r PAN_PANAL_r PAN_PCP_PANAL_r PRI_PASC_r PRI_PCP_PASC_r PVEM_PCP_r PVEM_PCP_PANAL_r PCP_PANAL_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT_PC PVEM PCP PANAL PASC PAN_PANAL PAN_PCP_PANAL PRI_PASC PRI_PCP_PASC PVEM_PCP PVEM_PCP_PANAL PCP_PANAL {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2006
gen month ="July"

sort section

save San_Luis_Potosi_Section_2006.dta,replace

**************************************************************************
**************************************************************************
**************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre_municipio  municipality
rename seccion section
rename lista_nominal listanominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring   listanominal nulos - total ,replace

collapse (sum)   listanominal nulos - total , by (municipality section)

**************************************************************************

foreach var in panpna panpcppna panptpna pnapsd prdpc prdpcp prdpcppc prdpt prdptpcp prdptpcppsd ptpc ptpcp ptpcppc ptpsd pvempcp pvempsd pripsd pripvem pripna {
bys municipality: egen c_`var' = sum(`var')
replace c_`var' = (c_`var'>0)
}

replace panpna = panpna + pan + panal if c_panpna==1
replace pan = 0 if c_panpna==1
replace panal = 0 if c_panpna==1
 
replace panpcppna = panpcppna + pan + pcp + panal if c_panpcppna==1
replace pan = 0 if c_panpcppna==1
replace panal = 0 if c_panpcppna==1
replace pcp = 0 if c_panpcppna==1
 
replace panptpna = panptpna + pan +pt +panal if c_panptpna==1
replace pan = 0 if c_panptpna==1
replace pt = 0 if c_panptpna==1
replace panal = 0 if c_panptpna==1
   
replace pnapsd =  pnapsd + psd + panal if c_pnapsd==1
replace psd = 0 if c_pnapsd==1
replace panal = 0 if c_pnapsd==1
 
replace prdpc = prdpc + prd + convergencia if c_prdpc ==1
replace prd =0 if c_prdpc ==1
replace convergencia =0 if c_prdpc ==1
 
replace prdpcp = prdpcp + prd +pcp if c_prdpcp ==1
replace prd =0 if c_prdpcp ==1
replace pcp =0 if c_prdpcp ==1
 
replace prdpcppc = prdpcppc + prd +pcp + convergencia if c_prdpcppc ==1
replace prd =0 if c_prdpcppc ==1
replace pcp =0 if c_prdpcppc ==1
replace convergencia =0 if c_prdpcppc ==1
 
replace prdpt = prdpt + prd +pt if c_prdpt ==1
replace prd =0 if c_prdpt==1
replace pt =0 if c_prdpt ==1 

replace prdptpcp = prdptpcp + prd +pt +pcp if c_prdptpcp ==1
replace prd =0 if c_prdptpcp==1
replace pt =0 if c_prdptpcp ==1
replace pcp =0 if c_prdptpcp ==1
    
replace prdptpcppsd = prdptpcppsd + prd +pt +pcp + psd if c_prdptpcppsd ==1
replace prd =0 if c_prdptpcppsd==1
replace pt =0 if c_prdptpcppsd ==1
replace pcp =0 if c_prdptpcppsd ==1
replace psd =0 if c_prdptpcppsd ==1

replace ptpc = ptpc + pt + convergencia if c_ptpc ==1
replace pt =0 if c_ptpc ==1
replace convergencia =0 if c_ptpc ==1
 
replace ptpcp = ptpcp + pt + pcp if c_ptpcp ==1
replace pt =0 if c_ptpcp ==1
replace pcp =0 if c_ptpcp ==1
 
replace ptpcppc = ptpcppc + pt + pcp + convergencia if c_ptpcppc ==1
replace pt =0 if c_ptpcppc ==1
replace pcp =0 if c_ptpcppc ==1
replace convergencia =0 if c_ptpcppc ==1
   
replace ptpsd = ptpsd + pt + psd if c_ptpsd ==1
replace pt =0 if c_ptpsd ==1
replace psd =0 if c_ptpsd ==1
  
replace pvempcp = pvempcp + pvem + pcp if c_pvempcp ==1
replace pvem =0 if c_pvempcp ==1
replace pcp =0 if c_pvempcp ==1
   
replace pvempsd = pvempsd + pvem + psd if c_pvempsd ==1
replace pvem =0 if c_pvempsd ==1
replace psd =0 if c_pvempsd ==1

replace pripna = pri + pripna + panal if c_pripna==1
replace pri = 0 if c_pripna==1
replace panal = 0 if c_pripna==1
 
replace pripsd = pri + pripsd +psd if c_pripsd == 1
replace pri =0 if c_pripsd == 1
replace psd =0 if c_pripsd == 1

replace pripvem = pri + pripvem  + pvem if c_pripvem ==1
replace pri =0 if c_pripvem ==1
replace pvem =0 if c_pripvem ==1

drop c_*

**************************************************************************
 
rename pan PAN
rename pri PRI
rename prd PRD
rename pt  PT
rename pvem PVEM
rename pcp PCP
rename convergencia PC
rename panal PANAL
rename psd PSD
rename panpna PAN_PANAL
rename panpcppna PAN_PCP_PANAL
rename panptpna PAN_PT_PANAL
rename pnapsd PANAL_PSD
rename prdpc PRD_PC
rename prdpcp PRD_PCP
rename prdpcppc PRD_PCP_PC
rename prdpt PRD_PT
rename prdptpcp PRD_PT_PCP
rename prdptpcppsd PRD_PT_PCP_PSD
rename ptpc PT_PC
rename ptpcp PT_PCP
rename ptpcppc PT_PCP_PC
rename ptpsd PT_PSD
rename pvempcp PVEM_PCP
rename pvempsd PVEM_PSD
rename pripna PRI_PANAL
rename pripsd PRI_PSD 
rename pripvem PRI_PVEM

gen turnout =  total/listanominal

drop   nulos 

gen   uniqueid= 0
replace uniqueid=24001 if municipality =="AHUALULCO"
replace uniqueid=24002 if municipality =="ALAQUINES"
replace uniqueid=24003 if municipality =="AQUISMON"
replace uniqueid=24004 if municipality =="ARMADILLO DE LOS INFANTE"
replace uniqueid=24053 if municipality =="AXTLA DE TERRAZAS"
replace uniqueid=24005 if municipality =="CARDENAS"
replace uniqueid=24006 if municipality =="CATORCE"
replace uniqueid=24007 if municipality =="CEDRAL"
replace uniqueid=24008 if municipality =="CERRITOS"
replace uniqueid=24009 if municipality =="CERRO DE SAN PEDRO"
replace uniqueid=24015 if municipality =="CHARCAS"
replace uniqueid=24010 if municipality =="CIUDAD DEL MAIZ"
replace uniqueid=24011 if municipality =="CIUDAD FERNANDEZ"
replace uniqueid=24013 if municipality =="CIUDAD VALLES"
replace uniqueid=24014 if municipality =="COXCATLAN"
replace uniqueid=24016 if municipality =="EBANO"
replace uniqueid=24058 if municipality =="EL NARANJO"
replace uniqueid=24017 if municipality =="GUADALCAZAR"
replace uniqueid=24018 if municipality =="HUEHUETLAN"
replace uniqueid=24019 if municipality =="LAGUNILLAS"
replace uniqueid=24020 if municipality =="MATEHUALA"
replace uniqueid=24057 if municipality =="MATLAPA"
replace uniqueid=24021 if municipality =="MEXQUITIC DE CARMONA"
replace uniqueid=24022 if municipality =="MOCTEZUMA"
replace uniqueid=24023 if municipality =="RAYON"
replace uniqueid=24024 if municipality =="RIOVERDE"
replace uniqueid=24025 if municipality =="SALINAS"
replace uniqueid=24026 if municipality =="SAN ANTONIO"
replace uniqueid=24027 if municipality =="SAN CIRO DE ACOSTA"
replace uniqueid=24028 if municipality =="SAN LUIS POTOSI"
replace uniqueid=24029 if municipality =="SAN MARTIN CHALCHICUAUTLA"
replace uniqueid=24030 if municipality =="SAN NICOLAS TOLENTINO"
replace uniqueid=24034 if municipality =="SAN VICENTE TANCUAYALAB"
replace uniqueid=24031 if municipality =="SANTA CATARINA"
replace uniqueid=24032 if municipality =="SANTA MARIA DEL RIO"
replace uniqueid=24033 if municipality =="SANTO DOMINGO"
replace uniqueid=24035 if municipality =="SOLEDAD DE GRACIANO SANCHEZ"
replace uniqueid=24036 if municipality =="TAMASOPO"
replace uniqueid=24037 if municipality =="TAMAZUNCHALE"
replace uniqueid=24038 if municipality =="TAMPACAN"
replace uniqueid=24039 if municipality =="TAMPAMOLON CORONA"
replace uniqueid=24040 if municipality =="TAMUIN"
replace uniqueid=24012 if municipality =="TANCANHUITZ"
replace uniqueid=24041 if municipality =="TANLAJAS"
replace uniqueid=24042 if municipality =="TANQUIAN DE ESCOBEDO"
replace uniqueid=24043 if municipality =="TIERRANUEVA"
replace uniqueid=24044 if municipality =="VANEGAS"
replace uniqueid=24045 if municipality =="VENADO"
replace uniqueid=24056 if municipality =="VILLA DE ARISTA"
replace uniqueid=24046 if municipality =="VILLA DE ARRIAGA"
replace uniqueid=24047 if municipality =="VILLA DE GUADALUPE"
replace uniqueid=24048 if municipality =="VILLA DE LA PAZ"
replace uniqueid=24049 if municipality =="VILLA DE RAMOS"
replace uniqueid=24050 if municipality =="VILLA DE REYES"
replace uniqueid=24051 if municipality =="VILLA HIDALGO"
replace uniqueid=24052 if municipality =="VILLA JUAREZ"
replace uniqueid=24054 if municipality =="XILITLA"
replace uniqueid=24055 if municipality =="ZARAGOZA"


egen valid = rowtotal(PAN PRI PRD PT PVEM PCP PC PANAL PSD PAN_PANAL PAN_PCP_PANAL PAN_PT_PANAL PANAL_PSD PRD_PC PRD_PCP PRD_PCP_PC PRD_PT PRD_PT_PCP PRD_PT_PCP_PSD PRI_PANAL PRI_PSD PRI_PVEM PT_PC PT_PCP PT_PCP_PC PT_PSD PVEM_PCP PVEM_PSD)

foreach var in PAN PRI PRD PT PVEM PCP PC PANAL PSD PAN_PANAL PAN_PCP_PANAL PAN_PT_PANAL PANAL_PSD PRD_PC PRD_PCP PRD_PCP_PC PRD_PT PRD_PT_PCP PRD_PT_PCP_PSD PRI_PANAL PRI_PSD PRI_PVEM PT_PC PT_PCP PT_PCP_PC PT_PSD PVEM_PCP PVEM_PSD total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PCP inv_mun_PC inv_mun_PANAL inv_mun_PSD inv_mun_PAN_PANAL inv_mun_PAN_PCP_PANAL inv_mun_PAN_PT_PANAL inv_mun_PANAL_PSD inv_mun_PRD_PC inv_mun_PRD_PCP inv_mun_PRD_PCP_PC inv_mun_PRD_PT inv_mun_PRD_PT_PCP inv_mun_PRD_PT_PCP_PSD inv_mun_PRI_PANAL inv_mun_PRI_PSD inv_mun_PRI_PVEM inv_mun_PT_PC inv_mun_PT_PCP inv_mun_PT_PCP_PC inv_mun_PT_PSD inv_mun_PVEM_PCP inv_mun_PVEM_PSD, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PCP_r PC_r PANAL_r PSD_r PAN_PANAL_r PAN_PCP_PANAL_r PAN_PT_PANAL_r PANAL_PSD_r PRD_PC_r PRD_PCP_r PRD_PCP_PC_r PRD_PT_r PRD_PT_PCP_r PRD_PT_PCP_PSD_r PRI_PANAL_r PRI_PSD_r PRI_PVEM_r PT_PC_r PT_PCP_r PT_PCP_PC_r PT_PSD_r PVEM_PCP_r PVEM_PSD_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PCP PC PANAL PSD PAN_PANAL PAN_PCP_PANAL PAN_PT_PANAL PANAL_PSD PRD_PC PRD_PCP PRD_PCP_PC PRD_PT PRD_PT_PCP PRD_PT_PCP_PSD PRI_PANAL PRI_PSD PRI_PVEM PT_PC PT_PCP PT_PCP_PC PT_PSD PVEM_PCP PVEM_PSD {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2009
gen month ="July"

save San_Luis_Potosi_Section_2009.dta,replace


**************************************************************************
**************************************************************************
**************************************************************************

use Ayu_Seccion_2012.dta, clear

gen turnout =  total/listanominal

replace municipality = trim(municipality)
gen   uniqueid= 0
replace uniqueid=24001 if strpos(municipality, "AHUALULCO")>0
replace uniqueid=24002 if strpos(municipality, "ALAQUINES")>0
replace uniqueid=24003 if strpos(municipality, "AQUISMON")>0
replace uniqueid=24004 if strpos(municipality, "ARMADILLO DE LOS INFANTE")>0
replace uniqueid=24053 if strpos(municipality, "AXTLA DE TERRAZAS")>0
replace uniqueid=24005 if strpos(municipality, "CARDENAS")>0
replace uniqueid=24006 if strpos(municipality, "CATORCE")>0
replace uniqueid=24007 if strpos(municipality, "CEDRAL")>0
replace uniqueid=24008 if strpos(municipality, "CERRITOS")>0
replace uniqueid=24009 if strpos(municipality, "CERRO DE SAN PEDRO")>0
replace uniqueid=24015 if strpos(municipality, "CHARCAS")>0
replace uniqueid=24010 if strpos(municipality, "CIUDAD DEL MAIZ")>0
replace uniqueid=24011 if strpos(municipality, "CIUDAD FERNANDEZ")>0
replace uniqueid=24013 if strpos(municipality, "CIUDAD VALLES")>0
replace uniqueid=24014 if strpos(municipality, "COXCATLAN")>0
replace uniqueid=24016 if strpos(municipality, "EBANO")>0
replace uniqueid=24058 if strpos(municipality, "EL NARANJO")>0
replace uniqueid=24017 if strpos(municipality, "GUADALCAZAR")>0
replace uniqueid=24018 if strpos(municipality, "HUEHUETLAN")>0
replace uniqueid=24019 if strpos(municipality, "LAGUNILLAS")>0
replace uniqueid=24020 if strpos(municipality, "MATEHUALA")>0
replace uniqueid=24057 if strpos(municipality, "MATLAPA")>0
replace uniqueid=24021 if strpos(municipality, "MEXQUITIC DE CARMONA")>0
replace uniqueid=24022 if strpos(municipality, "MOCTEZUMA")>0
replace uniqueid=24023 if strpos(municipality, "RAYON")>0
replace uniqueid=24024 if strpos(municipality, "RIOVERDE")>0
replace uniqueid=24025 if strpos(municipality, "SALINAS")>0
replace uniqueid=24026 if strpos(municipality, "SAN ANTONIO")>0
replace uniqueid=24027 if strpos(municipality, "SAN CIRO DE ACOSTA")>0
replace uniqueid=24028 if strpos(municipality, "SAN LUIS POTOSI")>0
replace uniqueid=24029 if strpos(municipality, "SAN MARTIN CHALCHICUAUTLA")>0
replace uniqueid=24030 if strpos(municipality, "SAN NICOLAS TOLENTINO")>0
replace uniqueid=24034 if strpos(municipality, "SAN VICENTE TANCUAYALAB")>0
replace uniqueid=24031 if strpos(municipality, "SANTA CATARINA")>0
replace uniqueid=24032 if strpos(municipality, "SANTA MARIA DEL RIO")>0
replace uniqueid=24033 if strpos(municipality, "SANTO DOMINGO")>0
replace uniqueid=24035 if strpos(municipality, "SOLEDAD DE GRACIANO SANCHEZ")>0
replace uniqueid=24036 if strpos(municipality, "TAMASOPO")>0
replace uniqueid=24037 if strpos(municipality, "TAMAZUNCHALE")>0
replace uniqueid=24038 if strpos(municipality, "TAMPACAN")>0
replace uniqueid=24039 if strpos(municipality, "TAMPAMOLON CORONA")>0
replace uniqueid=24040 if strpos(municipality, "TAMUIN")>0
replace uniqueid=24012 if strpos(municipality, "TANCANHUITZ")>0
replace uniqueid=24041 if strpos(municipality, "TANLAJAS")>0
replace uniqueid=24042 if strpos(municipality, "TANQUIAN DE ESCOBEDO")>0
replace uniqueid=24043 if strpos(municipality, "TIERRA NUEVA")>0
replace uniqueid=24044 if strpos(municipality, "VANEGAS")>0
replace uniqueid=24045 if strpos(municipality, "VENADO")>0
replace uniqueid=24056 if strpos(municipality, "VILLA DE ARISTA")>0
replace uniqueid=24046 if strpos(municipality, "VILLA DE ARRIAGA")>0
replace uniqueid=24047 if strpos(municipality, "VILLA DE GUADALUPE")>0
replace uniqueid=24048 if strpos(municipality, "VILLA DE LA PAZ")>0
replace uniqueid=24049 if strpos(municipality, "VILLA DE RAMOS")>0
replace uniqueid=24050 if strpos(municipality, "VILLA DE REYES")>0
replace uniqueid=24051 if strpos(municipality, "VILLA HIDALGO")>0
replace uniqueid=24052 if strpos(municipality, "VILLA JUAREZ")>0
replace uniqueid=24054 if strpos(municipality, "XILITLA")>0
replace uniqueid=24055 if strpos(municipality, "ZARAGOZA")>0

drop valid
egen valid = rowtotal(PAN PRI PRD PT PVEM PCP PC PANAL PAN_PANAL PRI_PVEM PRD_PT_PC PRD_PC PRD_PCP_PC PVEM_PCP PRD_PT PAN_PT_PC_PANAL PRD_PT_PCP_PC PT_PC PRI_PVEM_PCP PT_PCP_PC PRD_PCP PCP_PC PRI_PCP PAN_PRD_PT_PVEM_PCP_PC_PANAL)

foreach var in PAN PRI PRD PT PVEM PCP PC PANAL PAN_PANAL PRI_PVEM PRD_PT_PC PRD_PC PRD_PCP_PC PVEM_PCP PRD_PT PAN_PT_PC_PANAL PRD_PT_PCP_PC PT_PC PRI_PVEM_PCP PT_PCP_PC PRD_PCP PCP_PC PRI_PCP  total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

bys uniqueid: egen mun_long= sum(PAN_PRD_PT_PVEM_PCP_PC_PANAL) 
gen inv_mun_long= 1/mun_long

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PCP inv_mun_PC inv_mun_PANAL inv_mun_PAN_PANAL inv_mun_PRI_PVEM inv_mun_PRD_PT_PC inv_mun_PRD_PC inv_mun_PRD_PCP_PC inv_mun_PVEM_PCP inv_mun_PRD_PT inv_mun_PAN_PT_PC_PANAL inv_mun_PRD_PT_PCP_PC inv_mun_PT_PC inv_mun_PRI_PVEM_PCP inv_mun_PT_PCP_PC inv_mun_PRD_PCP inv_mun_PCP_PC inv_mun_PRI_PCP inv_mun_long, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PCP_r PC_r PANAL_r PAN_PANAL_r PRI_PVEM_r PRD_PT_PC_r PRD_PC_r PRD_PCP_PC_r PVEM_PCP_r PRD_PT_r PAN_PT_PC_PANAL_r PRD_PT_PCP_PC_r PT_PC_r PRI_PVEM_PCP_r PT_PCP_PC_r PRD_PCP_r PCP_PC_r PRI_PCP_r long_r)
drop inv_mun_*
                

gen winner = ""
gen second = ""
gen third = ""


foreach var in PAN PRI PRD PT PVEM PCP PC PANAL PAN_PANAL PRI_PVEM PRD_PT_PC PRD_PC PRD_PCP_PC PVEM_PCP PRD_PT PAN_PT_PC_PANAL PRD_PT_PCP_PC PT_PC PRI_PVEM_PCP PT_PCP_PC PRD_PCP PCP_PC PRI_PCP  {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

foreach var in PAN_PRD_PT_PVEM_PCP_PC_PANAL {

replace winner = "`var'" if long_r == 1
replace second = "`var'" if long_r == 2
replace third = "`var'" if long_r == 3

}  
  
  
  
drop *_r

gen year = 2012
gen month ="July"

sort section

save San_Luis_Potosi_Section_2012.dta,replace

**************************************************************************
**************************************************************************
**************************************************************************

clear all
set mem 1g

use San_Luis_Potosi_Section_1997.dta
append using San_Luis_Potosi_Section_2000.dta
append using San_Luis_Potosi_Section_2003.dta
append using San_Luis_Potosi_Section_2006.dta
append using San_Luis_Potosi_Section_2009.dta
append using San_Luis_Potosi_Section_2012.dta

erase San_Luis_Potosi_Section_1997.dta
erase San_Luis_Potosi_Section_2000.dta
erase San_Luis_Potosi_Section_2003.dta
erase San_Luis_Potosi_Section_2006.dta
erase San_Luis_Potosi_Section_2009.dta
erase San_Luis_Potosi_Section_2012.dta

tab winner, missing
tab year if winner==""
tab municipality if winner==""

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

gen PC_winner =0
replace PC_winner =1 if strpos(winner, "PC")>0 &  (strpos(winner, "PC")!=strpos(winner, "PCP"))
replace winner_counter = winner_counter + PC_winner

foreach var in PRI PRD PT PCP PVEM PANAL PD PSD PAS PSN PartidoMexicoPosible CDPPN PUP PEC NPP {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

replace winner_counter = 2 if winner=="PCP_PC"
replace winner_counter = 3 if winner=="PT_PCP_PC"
replace winner_counter = 7 if winner=="PAN_PRD_PT_PVEM_PCP_PC_PANAL"

tab winner_counter
count if winner_counter==0

save San_Luis_Potosi_ALL.dta,replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save San_Luis_Potosi_ALL.dta,replace
