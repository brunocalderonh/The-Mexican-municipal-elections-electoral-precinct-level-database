

cd "C:\Users\Horacio\Dropbox\Audits\Election Data\San Luis Potosi - 2003, 2006, 2009, 2012\CASILLAS_AYUNTAMIENTOS_2012"

foreach filename in Ayto_Ahualulco Ayto_Alaquines Ayto_Aquismon Ayto_Armadillo_de_los_Infante Ayto_Axtla_de_Terrazas Ayto_Cardenas Ayto_Catorce Ayto_Cedral Ayto_Cerritos Ayto_Cerro_de_San_Pedro Ayto_Charcas Ayto_Ciudad_del_Maiz Ayto_Ciudad_Fernandez Ayto_Ciudad_Valles Ayto_Coxcatlan Ayto_Ebano Ayto_El_Naranjo Ayto_Guadalcazar Ayto_Huehuetlan Ayto_Lagunillas Ayto_Matehuala Ayto_Matlapa Ayto_Mexquitic_de_Carmona Ayto_Moctezuma Ayto_Rayon Ayto_Rioverde Ayto_Salinas Ayto_San_Antonio Ayto_San_Ciro_de_Acosta Ayto_San_Luis_Potosi Ayto_San_Martin_Chalchicuautla Ayto_San_Nicolas_Tolentino Ayto_San_Vicente_Tancuayalab Ayto_Santa_Catarina Ayto_Santa_Maria_del_Rio Ayto_Santo_Domingo Ayto_Soledad_de_Graciano_Sanchez Ayto_Tamasopo Ayto_Tamazunchale Ayto_Tampacan Ayto_Tampamolon Ayto_Tamuin Ayto_Tancanhuitz Ayto_Tanlajas Ayto_Tanquian_de_Escobedo Ayto_Tierranueva Ayto_Vanegas Ayto_Venado Ayto_Villa_de_Arista Ayto_Villa_de_Arriaga Ayto_Villa_de_Guadalupe Ayto_Villa_de_la_Paz Ayto_Villa_de_Ramos Ayto_Villa_de_Reyes Ayto_Villa_Hidalgo Ayto_Villa_Juarez Ayto_Xilitla Ayto_Zaragoza {
insheet using `filename'.csv, clear
save `filename'.dta, replace
}

clear 
foreach filename in Ayto_Ahualulco Ayto_Alaquines Ayto_Aquismon Ayto_Armadillo_de_los_Infante Ayto_Axtla_de_Terrazas Ayto_Cardenas Ayto_Catorce Ayto_Cedral Ayto_Cerritos Ayto_Cerro_de_San_Pedro Ayto_Charcas Ayto_Ciudad_del_Maiz Ayto_Ciudad_Fernandez Ayto_Ciudad_Valles Ayto_Coxcatlan Ayto_Ebano Ayto_El_Naranjo Ayto_Guadalcazar Ayto_Huehuetlan Ayto_Lagunillas Ayto_Matehuala Ayto_Matlapa Ayto_Mexquitic_de_Carmona Ayto_Moctezuma Ayto_Rayon Ayto_Rioverde Ayto_Salinas Ayto_San_Antonio Ayto_San_Ciro_de_Acosta Ayto_San_Luis_Potosi Ayto_San_Martin_Chalchicuautla Ayto_San_Nicolas_Tolentino Ayto_San_Vicente_Tancuayalab Ayto_Santa_Catarina Ayto_Santa_Maria_del_Rio Ayto_Santo_Domingo Ayto_Soledad_de_Graciano_Sanchez Ayto_Tamasopo Ayto_Tamazunchale Ayto_Tampacan Ayto_Tampamolon Ayto_Tamuin Ayto_Tancanhuitz Ayto_Tanlajas Ayto_Tanquian_de_Escobedo Ayto_Tierranueva Ayto_Vanegas Ayto_Venado Ayto_Villa_de_Arista Ayto_Villa_de_Arriaga Ayto_Villa_de_Guadalupe Ayto_Villa_de_la_Paz Ayto_Villa_de_Ramos Ayto_Villa_de_Reyes Ayto_Villa_Hidalgo Ayto_Villa_Juarez Ayto_Xilitla Ayto_Zaragoza {
append using `filename'.dta
erase `filename'.dta
}

rename municipio municipality
rename nodeseccion section
drop tipo dtolocal nommpio formulasnoregistradas votosnulos

rename votacionvalidaemitida  valid
rename votacionemitida total

rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pvem  PVEM
rename pcp PCP
rename pmc PC
rename pna PANAL

rename cc_pan_pna CC_PAN_PANAL
rename cc_pri_pvem CC_PRI_PVEM
rename cc_prd_pt_pmc CC_PRD_PT_PC
rename cc_prd_pmc CC_PRD_PC
rename cc_prd_pcp_pmc CC_PRD_PCP_PC
rename cc_pvem_pcp CC_PVEM_PCP
rename cc_prd_pt CC_PRD_PT
rename cc_prd_pt_pcp_pmc CC_PRD_PT_PCP_PC
rename cc_pt_pmc CC_PT_PC
rename cc_pri_pvem_pcp CC_PRI_PVEM_PCP 
rename cc_pan_pt_pmc_pna CC_PAN_PT_PC_PANAL
rename cc_pt_pcp_pmc CC_PT_PCP_PC
rename cc_prd_pcp CC_PRD_PCP
rename cc_pcp_pmc CC_PCP_PC
rename cc_pri_pcp CC_PRI_PCP
rename cc_pan_prd_pt_pvem_pcp_pmc_pna CC_PAN_PRD_PT_PVEM_PCP_PC_PANAL

rename cc_1 CC_1
rename cc_2 CC_2
rename cc_3 CC_3

rename pan_pna PAN_PANAL
rename pri_pvem PRI_PVEM
rename prd_pt_pmc PRD_PT_PC
rename prd_pmc PRD_PC
rename prd_pcp_pmc PRD_PCP_PC
rename pvem_pcp PVEM_PCP
rename prd_pt PRD_PT
rename prd_pt_pcp_pmc PRD_PT_PCP_PC
rename pt_pmc PT_PC
rename pri_pvem_pcp PRI_PVEM_PCP
rename pan_pt_pmc_pna PAN_PT_PC_PANAL
rename pt_pcp_pmc PT_PCP_PC
rename prd_pcp PRD_PCP
rename pcp_pmc PCP_PC
rename pri_pcp PRI_PCP
rename pan_prd_pt_pvem_pcp_pmc_pna PAN_PRD_PT_PVEM_PCP_PC_PANAL

collapse (sum) listanominal - PRD_PT_PC CC_PRD_PC - PAN_PRD_PT_PVEM_PCP_PC_PANAL, by(municipality section CC_1 CC_2 CC_3)

replace CC_1 = subinstr(CC_1, "PNA", "PANAL", .)

gen test = PAN_PANAL - CC_PAN_PANAL  - PAN - PANAL if CC_1=="PAN_PANAL" | CC_2=="PAN_PANAL" | CC_3=="PAN_PANAL"
tab test
replace PAN = 0   if CC_1=="PAN_PANAL" | CC_2=="PAN_PANAL" | CC_3=="PAN_PANAL"
replace PANAL = 0 if CC_1=="PAN_PANAL" | CC_2=="PAN_PANAL" | CC_3=="PAN_PANAL"
drop test CC_PAN_PANAL

gen test  = PAN_PRD_PT_PVEM_PCP_PC_PANAL - CC_PAN_PRD_PT_PVEM_PCP_PC_PANAL - PAN - PRD - PT - PVEM - PCP - PC - PANAL if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
tab test
replace PAN = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PRD = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PT = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PVEM = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PCP = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PC = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
replace PANAL = 0 if CC_1=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_2=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL" | CC_3=="PAN_PRD_PT_PVEM_PCP_PMC_PANAL"
drop test CC_PAN_PRD_PT_PVEM_PCP_PC_PANAL

gen test = PAN_PT_PC_PANAL - CC_PAN_PT_PC_PANAL - PAN - PT - PC - PANAL if CC_1=="PAN_PT_PMC_PANAL" | CC_2=="PAN_PT_PMC_PANAL" | CC_3=="PAN_PT_PMC_PANAL"
tab test 
replace PAN = 0  if CC_1=="PAN_PT_PMC_PANAL" | CC_2=="PAN_PT_PMC_PANAL" | CC_3=="PAN_PT_PMC_PANAL"
replace PT = 0  if CC_1=="PAN_PT_PMC_PANAL" | CC_2=="PAN_PT_PMC_PANAL" | CC_3=="PAN_PT_PMC_PANAL"
replace PC = 0  if CC_1=="PAN_PT_PMC_PANAL" | CC_2=="PAN_PT_PMC_PANAL" | CC_3=="PAN_PT_PMC_PANAL"
replace PANAL = 0  if CC_1=="PAN_PT_PMC_PANAL" | CC_2=="PAN_PT_PMC_PANAL" | CC_3=="PAN_PT_PMC_PANAL"
drop test CC_PAN_PT_PC_PANAL

gen test = PCP_PC - CC_PCP_PC - PCP - PC if CC_1=="PCP_PMC" | CC_2=="PCP_PMC" | CC_3=="PCP_PMC"
tab test 
replace PCP = 0 if CC_1=="PCP_PMC" | CC_2=="PCP_PMC" | CC_3=="PCP_PMC"
replace PC = 0 if CC_1=="PCP_PMC" | CC_2=="PCP_PMC" | CC_3=="PCP_PMC"
drop test CC_PCP_PC

gen test = PRD_PCP -  CC_PRD_PCP - PRD - PCP if CC_1=="PRD_PCP" | CC_2=="PRD_PCP" | CC_3=="PRD_PCP"
tab test 
replace PRD = 0 if CC_1=="PRD_PCP" | CC_2=="PRD_PCP" | CC_3=="PRD_PCP"
replace PCP = 0 if CC_1=="PRD_PCP" | CC_2=="PRD_PCP" | CC_3=="PRD_PCP"
drop test CC_PRD_PCP

gen test = PRD_PC - CC_PRD_PC - PRD - PC if CC_1=="PRD_PMC" | CC_2=="PRD_PMC" | CC_3=="PRD_PMC"
tab test 
replace PRD = 0 if CC_1=="PRD_PMC" | CC_2=="PRD_PMC" | CC_3=="PRD_PMC"
replace PC = 0 if CC_1=="PRD_PMC" | CC_2=="PRD_PMC" | CC_3=="PRD_PMC"
drop test CC_PRD_PC

gen test = PRD_PCP_PC - CC_PRD_PCP_PC  - PRD - PCP - PC  if CC_1=="PRD_PCP_PMC" | CC_2=="PRD_PCP_PMC" | CC_3=="PRD_PCP_PMC"
tab test 
replace PRD = 0 if CC_1=="PRD_PCP_PMC" | CC_2=="PRD_PCP_PMC" | CC_3=="PRD_PCP_PMC"
replace PCP = 0 if CC_1=="PRD_PCP_PMC" | CC_2=="PRD_PCP_PMC" | CC_3=="PRD_PCP_PMC"
replace PC = 0 if CC_1=="PRD_PCP_PMC" | CC_2=="PRD_PCP_PMC" | CC_3=="PRD_PCP_PMC"
drop test CC_PRD_PCP_PC

gen test = PRD_PT - CC_PRD_PT - PRD - PT if CC_1=="PRD_PT" | CC_2=="PRD_PT" | CC_3=="PRD_PT"
tab test 
replace PRD = 0  if CC_1=="PRD_PT" | CC_2=="PRD_PT" | CC_3=="PRD_PT"
replace PT = 0  if CC_1=="PRD_PT" | CC_2=="PRD_PT" | CC_3=="PRD_PT"
drop test CC_PRD_PT

gen test = PRD_PT_PCP_PC - CC_PRD_PT_PCP_PC - PRD - PT - PCP - PC if CC_1=="PRD_PT_PCP_PMC" | CC_2=="PRD_PT_PCP_PMC" | CC_3=="PRD_PT_PCP_PMC"
tab test 
replace PRD = 0  if CC_1=="PRD_PT_PCP_PMC" | CC_2=="PRD_PT_PCP_PMC" | CC_3=="PRD_PT_PCP_PMC"
replace PT = 0  if CC_1=="PRD_PT_PCP_PMC" | CC_2=="PRD_PT_PCP_PMC" | CC_3=="PRD_PT_PCP_PMC"
replace PCP = 0  if CC_1=="PRD_PT_PCP_PMC" | CC_2=="PRD_PT_PCP_PMC" | CC_3=="PRD_PT_PCP_PMC"
replace PC = 0  if CC_1=="PRD_PT_PCP_PMC" | CC_2=="PRD_PT_PCP_PMC" | CC_3=="PRD_PT_PCP_PMC"
drop test CC_PRD_PT_PCP_PC

gen test = PRD_PT_PC - CC_PRD_PT_PC - PRD - PT - PC if CC_1=="PRD_PT_PMC" | CC_2=="PRD_PT_PMC" | CC_3=="PRD_PT_PMC"
replace PRD = 0 if CC_1=="PRD_PT_PMC" | CC_2=="PRD_PT_PMC" | CC_3=="PRD_PT_PMC"
replace PT = 0 if CC_1=="PRD_PT_PMC" | CC_2=="PRD_PT_PMC" | CC_3=="PRD_PT_PMC"
replace PC = 0 if CC_1=="PRD_PT_PMC" | CC_2=="PRD_PT_PMC" | CC_3=="PRD_PT_PMC"
tab test 
drop test CC_PRD_PT_PC

gen test = PRI_PCP - CC_PRI_PCP - PRI - PCP if CC_1=="PRI_PCP" | CC_2=="PRI_PCP" | CC_3=="PRI_PCP"
tab test 
replace PRI = 0 if CC_1=="PRI_PCP" | CC_2=="PRI_PCP" | CC_3=="PRI_PCP"
replace PCP = 0 if CC_1=="PRI_PCP" | CC_2=="PRI_PCP" | CC_3=="PRI_PCP"
drop test CC_PRI_PCP

gen test = PRI_PVEM - CC_PRI_PVEM - PRI - PVEM if CC_1=="PRI_PVEM" | CC_2=="PRI_PVEM" | CC_3=="PRI_PVEM"
tab test 
replace PRI = 0 if CC_1=="PRI_PVEM" | CC_2=="PRI_PVEM" | CC_3=="PRI_PVEM"
replace PVEM = 0 if CC_1=="PRI_PVEM" | CC_2=="PRI_PVEM" | CC_3=="PRI_PVEM"
drop test CC_PRI_PVEM

gen test = PRI_PVEM_PCP - CC_PRI_PVEM_PCP - PRI - PVEM - PCP if CC_1=="PRI_PVEM_PCP" | CC_2=="PRI_PVEM_PCP" | CC_3=="PRI_PVEM_PCP"
tab test 
replace PRI = 0 if CC_1=="PRI_PVEM_PCP" | CC_2=="PRI_PVEM_PCP" | CC_3=="PRI_PVEM_PCP"
replace PVEM = 0 if CC_1=="PRI_PVEM_PCP" | CC_2=="PRI_PVEM_PCP" | CC_3=="PRI_PVEM_PCP"
replace PCP = 0 if CC_1=="PRI_PVEM_PCP" | CC_2=="PRI_PVEM_PCP" | CC_3=="PRI_PVEM_PCP"
drop test CC_PRI_PVEM_PCP

gen test = PT_PCP_PC - CC_PT_PCP_PC - PT - PCP - PC if CC_1=="PT_PCP_PMC" | CC_2=="PT_PCP_PMC" | CC_3=="PT_PCP_PMC"
tab test 
replace PT = 0 if CC_1=="PT_PCP_PMC" | CC_2=="PT_PCP_PMC" | CC_3=="PT_PCP_PMC"
replace PCP = 0 if CC_1=="PT_PCP_PMC" | CC_2=="PT_PCP_PMC" | CC_3=="PT_PCP_PMC"
replace PC = 0 if CC_1=="PT_PCP_PMC" | CC_2=="PT_PCP_PMC" | CC_3=="PT_PCP_PMC" 
drop test CC_PT_PCP_PC

gen test = PT_PC  - CC_PT_PC - PT - PC if CC_1=="PT_PMC" | CC_2=="PT_PMC" | CC_3=="PT_PMC"
tab test 
replace PT = 0 if CC_1=="PT_PMC" | CC_2=="PT_PMC" | CC_3=="PT_PMC"
replace PC = 0 if CC_1=="PT_PMC" | CC_2=="PT_PMC" | CC_3=="PT_PMC"
drop test CC_PT_PC

gen test = PVEM_PCP - CC_PVEM_PCP - PVEM - PCP if CC_1=="PVEM_PCP" | CC_2=="PVEM_PCP" | CC_3=="PVEM_PCP"
tab test 
replace PVEM = 0 if CC_1=="PVEM_PCP" | CC_2=="PVEM_PCP" | CC_3=="PVEM_PCP"
replace PCP = 0 if CC_1=="PVEM_PCP" | CC_2=="PVEM_PCP" | CC_3=="PVEM_PCP"
drop test CC_PVEM_PCP

d CC_*
drop CC_*

cd "C:\Users\Horacio\Dropbox\Audits\Election Data\San Luis Potosi - 2003, 2006, 2009, 2012"

save Ayu_Seccion_2012.dta, replace

