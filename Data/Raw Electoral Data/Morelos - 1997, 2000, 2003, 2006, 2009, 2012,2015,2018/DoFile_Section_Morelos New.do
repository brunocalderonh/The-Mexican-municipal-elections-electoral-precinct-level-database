
clear all 
set mem 1g

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Morelos - 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "C:\Users\hlarreguy\Dropbox\Audits\Election Data\Morelos - 1997, 2000, 2003, 2006, 2009, 2012"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Data Analysis/Raw Data/Precinct/Morelos - 1997, 2000, 2003, 2006, 2009, 2012"

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************
insheet using Ayu_Seccion_1997_No_LN.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

egen total = rowtotal(pan pri prd pc pt pvem pcm pps  pdm  noregistrados  nulos)
drop if total==. | total==0 

destring pan -  nulos, replace

collapse (sum) pan-  nulos total, by (municipality section)
  
rename  pan    PAN
rename  pri    PRI
rename  prd    PRD
rename  pc     Partido_Cardenista
rename  pt     PT
rename  pvem   PVEM
rename  pcm    PCM
rename  pps    PPS
rename  pdm    PDM

* gen turnout =  total/listanominal

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="Axochiapan"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIO"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="Jantetelco"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCAN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN DE AMILPAS"


egen valid = rowtotal(PAN PRI PRD Partido_Cardenista PT PVEM PCM PPS PDM)

foreach var in PAN PRI PRD Partido_Cardenista PT PVEM PCM PPS PDM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_Partido_Cardenista inv_mun_PT inv_mun_PVEM inv_mun_PCM inv_mun_PPS inv_mun_PDM, gen(PAN_r PRI_r PRD_r Partido_Cardenista_r PT_r PVEM_r PCM_r PPS_r PDM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD Partido_Cardenista PT PVEM PCM PPS PDM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 1997
gen month ="March"

save Morelos_Section_1997.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2000.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.

egen total = rowtotal(pan pri apm pt pvem pcm parm pds pas noreg nulos)
drop if total==. | total==0 

destring  listanominal pan -  nulos total, replace

collapse (sum)  listanominal pan-  nulos total, by (municipality section)
  
rename pan PAN
rename pri PRI
rename apm  PRD_PC_PCD_PSN
rename pt PT
rename pvem  PVEM
rename pcm  PCM
rename parm  PARM
rename pds  PDS
rename pas  PAS

gen turnout =  total/listanominal

drop    noreg nulos 

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="AXOCHIAPAN"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIO"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="JANTETELCO"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCAN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN"


egen valid = rowtotal(PAN PRI PRD_PC_PCD_PSN PT PVEM PCM PARM PDS PAS)

foreach var in PAN PRI PRD_PC_PCD_PSN PT PVEM PCM PARM PDS PAS  listanominal total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PC_PCD_PSN inv_mun_PT inv_mun_PVEM inv_mun_PCM inv_mun_PARM inv_mun_PDS inv_mun_PAS, gen(PAN_r PRI_r PRD_PC_PCD_PSN_r PT_r PVEM_r PCM_r PARM_r PDS_r PAS_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PC_PCD_PSN PT PVEM PCM PARM PDS PAS {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2000
gen month ="July"

save Morelos_Section_2000.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************


insheet using Ayu_Seccion_2003_No_LN.csv, clear

rename municipio  municipality
rename seccin section
* rename listanominal nominal

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan -  total, replace

collapse (sum) pan-  fc total, by (municipality section)

rename pan    PAN
rename pri    PRI
rename prd    PRD
rename udemor UDM
rename pvem   PVEM
rename c      PC
rename psn    PSN
rename pas    PAS
* Partido Mexico Posible
rename mp     PMP
rename plm    PLM
rename fc     PFC
 
* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="AXOCHIAPAN"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIÓ"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="JANTETELCO"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCÁN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN DE AMILPAS"

egen valid = rowtotal(PAN PRI PRD UDM PVEM PC PSN PAS PMP PLM PFC)

foreach var in PAN PRI PRD UDM PVEM PC PSN PAS PMP PLM PFC total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

* gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_UDM inv_mun_PVEM inv_mun_PC inv_mun_PSN inv_mun_PAS inv_mun_PMP inv_mun_PLM inv_mun_PFC, gen(PAN_r PRI_r PRD_r UDM_r PVEM_r PC_r PSN_r PAS_r PMP_r PLM_r PFC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD UDM PVEM PC PSN PAS PMP PLM PFC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2003
gen month ="July"

sort section

save Morelos_Section_2003.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2006.csv, clear

rename municipio  municipality
rename seccion section
drop if municipality=="" & section==.
egen total =  rowtotal(pan pri prdptpc pvem pna alternativa  cc noregistrados nulos)
drop if total==. | total==0 

destring  listanominal pan - nulos, replace

collapse (sum) listanominal pan - cc total, by (municipality section)

*************************************************************************************************

* Candidaturas Comunes:
* PVEM-PNA= CUAUTLA
* PRI-PVEM=JIUTEPEC, PUENTE DE IXTLA, TOTOLAPAN Y ZACUALPAN

gen pvem_pna = 0
replace pvem_pna = cc + pvem + pna if municipality=="CUAUTLA"
replace pvem = 0 if municipality=="CUAUTLA"
replace pna = 0 if municipality=="CUAUTLA"
replace cc = 0 if municipality=="CUAUTLA"

gen pri_pvem = pri + pvem + cc if municipality=="JIUTEPEC" | municipality=="PUENTE DE IXTLA"  | municipality=="TOTOLAPAN"  | municipality=="ZACUALPAN"  
replace pri = 0 if municipality=="JIUTEPEC" | municipality=="PUENTE DE IXTLA"  | municipality=="TOTOLAPAN"  | municipality=="ZACUALPAN"  
replace pvem = 0 if municipality=="JIUTEPEC" | municipality=="PUENTE DE IXTLA"  | municipality=="TOTOLAPAN"  | municipality=="ZACUALPAN"  
replace cc = 0  if municipality=="JIUTEPEC" | municipality=="PUENTE DE IXTLA"  | municipality=="TOTOLAPAN"  | municipality=="ZACUALPAN"  

tab cc
drop cc

*************************************************************************************************
rename pan PAN
rename pri PRI
rename prdptpc  PRD_PT_PC
rename pvem PVEM
rename pna  PANAL
rename alternativa  PAS
rename pvem_pna  PVEM_PANAL
rename pri_pvem  PRI_PVEM

*************************************************************************************************

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="AXOCHIAPAN"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIO"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="JANTETELCO"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCAN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN"

egen valid = rowtotal(PAN PRI PRD_PT_PC PVEM PANAL PAS PVEM_PANAL PRI_PVEM)

foreach var in PAN PRI PRD_PT_PC PVEM PANAL PAS PVEM_PANAL PRI_PVEM total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD_PT_PC inv_mun_PVEM inv_mun_PANAL inv_mun_PAS inv_mun_PVEM_PANAL inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_PT_PC_r PVEM_r PANAL_r PAS_r PVEM_PANAL_r PRI_PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD_PT_PC PVEM PANAL PAS PVEM_PANAL PRI_PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2006
gen month ="July"

sort section

save Morelos_Section_2006.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2009.csv, clear

rename nombre_municipio  municipality
rename seccion section
drop if municipality=="" & section==.
drop if total==. | total==0 

destring  listanominal nulos - total, replace

collapse (sum) listanominal pan - total, by (municipality section)

*************************************************************************************************

* PRI-C		OCUITUCO
* PAN-PSD	TETELA DEL VOLCAN
* PRD-PT	CUERNAVACA;	XOCHITEPEC
* PRD-C	    AXOCHIAPAN;	JANTETELCO;	JONACATEPEC;	TEPALCINGO;	TEPOZTLAN
* PRD-PT-C	JIUTEPEC

gen dummy_pri_pc = (municipality=="OCUITUCO")
gen dummy_pan_psd = (municipality=="TETELA DEL VOLCAN")
gen dummy_prd_pt = (municipality=="CUERNAVACA" | municipality=="XOCHITEPEC")
gen dummy_prd_pc = (municipality=="AXOCHIAPAN" | municipality=="AXOCHIAPAN"  | municipality=="JANTETELCO"  | municipality=="JONACATEPEC"  | municipality=="TEPALCINGO"  | municipality=="TEPOZTLAN")
gen dummy_prd_pt_pc = (municipality=="JIUTEPEC")

replace panpsd =  panpsd + pan + psd if dummy_pan_psd==1
replace pan =0 if dummy_pan_psd==1
replace psd =0 if dummy_pan_psd==1
drop dummy_pan_psd

replace pripc =  pripc + pri + pc if dummy_pri_pc==1
replace pri =0 if dummy_pri_pc==1
replace pc  =0 if dummy_pri_pc==1
drop dummy_pri_pc

replace prdpt =  prdpt + prd + pt if dummy_prd_pt==1
replace prd =0 if dummy_prd_pt==1
replace pt  =0 if dummy_prd_pt==1
drop dummy_prd_pt

replace prdpc =  prdpc + prd + pc if dummy_prd_pc==1
replace prd =0 if dummy_prd_pc==1
replace pc  =0 if dummy_prd_pc==1
drop dummy_prd_pc

replace prdptpc =  prdptpc + prd + pt + pc  if dummy_prd_pt_pc==1
replace prd =0 if dummy_prd_pt_pc==1
replace pt  =0 if dummy_prd_pt_pc==1
replace pc  =0 if dummy_prd_pt_pc==1
drop dummy_prd_pt
 
*************************************************************************************************

rename pan PAN
rename pri PRI
rename prd  PRD
rename pt  PT
rename pvem  PVEM
rename pc PC
rename panal  PANAL
rename psd  PSD
rename panpsd PAN_PSD
rename prdpc PRD_PC
rename prdpt PRD_PT
rename prdptpc PRD_PT_PC
rename pripc PRI_PC

gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="AXOCHIAPAN"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIO"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="JANTETELCO"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCAN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PANAL PSD PAN_PSD PRI_PC PRD_PT PRD_PC PRD_PT_PC)

foreach var in PAN PRI PRD PT PVEM PC PANAL PSD PAN_PSD PRI_PC PRD_PT PRD_PC PRD_PT_PC total listanominal valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PANAL inv_mun_PSD inv_mun_PAN_PSD inv_mun_PRI_PC inv_mun_PRD_PT inv_mun_PRD_PC inv_mun_PRD_PT_PC, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PANAL_r PSD_r PAN_PSD_r PRI_PC_r PRD_PT_r PRD_PC_r PRD_PT_PC_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PVEM PC PANAL PSD PAN_PSD PRI_PC PRD_PT PRD_PC PRD_PT_PC {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2009
gen month ="July"

sort section

save Morelos_Section_2009.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

insheet using Ayu_Seccion_2012.csv, clear

replace municipality  = subinstr(municipality, "*", "", .) 
drop if municipality==""

collapse (sum)  pan - nulos, by(municipality section)

drop pri_panal prd_pt_pc prd_pc pvem_panal pri_pvem
rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pc PC
rename pvem PVEM
rename panal PANAL
rename psd PSD
replace c_pri_panal = cc_pri_panal if c_pri_panal==0 & cc_pri_panal!=0
drop cc_pri_panal 
rename c_pri_panal PRI_PANAL
rename c_prd_pt_pc  PRD_PT_PC
rename c_prd_pc  PRD_PC
rename c_pvem_panal  PVEM_PANAL
rename c_pri_pvem PRI_PVEM

egen total = rowtotal(PAN PRI PRD PT PC PVEM PANAL PSD PRI_PANAL PRD_PT_PC PRD_PC PVEM_PANAL PRI_PVEM nulos)
drop nulos

* gen turnout =  total/listanominal

gen   uniqueid= 0
replace uniqueid=17001 if municipality =="AMACUZAC"
replace uniqueid=17002 if municipality =="ATLATLAHUCAN"
replace uniqueid=17003 if municipality =="AXOCHIAPAN"
replace uniqueid=17004 if municipality =="AYALA"
replace uniqueid=17005 if municipality =="COATLAN DEL RIO"
replace uniqueid=17006 if municipality =="CUAUTLA"
replace uniqueid=17007 if municipality =="CUERNAVACA"
replace uniqueid=17008 if municipality =="EMILIANO ZAPATA"
replace uniqueid=17009 if municipality =="HUITZILAC"
replace uniqueid=17010 if municipality =="JANTETELCO"
replace uniqueid=17011 if municipality =="JIUTEPEC"
replace uniqueid=17012 if municipality =="JOJUTLA"
replace uniqueid=17013 if municipality =="JONACATEPEC"
replace uniqueid=17014 if municipality =="MAZATEPEC"
replace uniqueid=17015 if municipality =="MIACATLAN"
replace uniqueid=17016 if municipality =="OCUITUCO"
replace uniqueid=17017 if municipality =="PUENTE DE IXTLA"
replace uniqueid=17018 if municipality =="TEMIXCO"
replace uniqueid=17033 if municipality =="TEMOAC"
replace uniqueid=17019 if municipality =="TEPALCINGO"
replace uniqueid=17020 if municipality =="TEPOZTLAN"
replace uniqueid=17021 if municipality =="TETECALA"
replace uniqueid=17022 if municipality =="TETELA DEL VOLCAN"
replace uniqueid=17023 if municipality =="TLALNEPANTLA"
replace uniqueid=17024 if municipality =="TLALTIZAPAN"
replace uniqueid=17025 if municipality =="TLAQUILTENANGO"
replace uniqueid=17026 if municipality =="TLAYACAPAN"
replace uniqueid=17027 if municipality =="TOTOLAPAN"
replace uniqueid=17028 if municipality =="XOCHITEPEC"
replace uniqueid=17029 if municipality =="YAUTEPEC"
replace uniqueid=17030 if municipality =="YECAPIXTLA"
replace uniqueid=17031 if municipality =="ZACATEPEC"
replace uniqueid=17032 if municipality =="ZACUALPAN"

egen valid = rowtotal(PAN PRI PRD PT PC PVEM PANAL PSD PRI_PANAL PRD_PT_PC PRD_PC PVEM_PANAL PRI_PVEM)

foreach var in PAN PRI PRD PT PC PVEM PANAL PSD PRI_PANAL PRD_PT_PC PRD_PC PVEM_PANAL PRI_PVEM total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

*gen mun_turnout =  mun_total/mun_listanominal

replace PRI = . if mun_PRI_PANAL>0
replace PANAL = . if mun_PRI_PANAL>0
replace PRD = . if mun_PRD_PT_PC>0
replace PT = . if mun_PRD_PT_PC>0
replace PC = . if mun_PRD_PT_PC>0
replace PRD = . if mun_PRD_PC>0
replace PC = . if mun_PRD_PC>0
replace PVEM = . if mun_PVEM_PANAL>0
replace PANAL = . if mun_PVEM_PANAL>0
replace PRI = . if mun_PRI_PVEM>0
replace PVEM = . if mun_PRI_PVEM>0

replace inv_mun_PRI = . if mun_PRI_PANAL>0
replace inv_mun_PANAL = . if mun_PRI_PANAL>0
replace inv_mun_PRD = . if mun_PRD_PT_PC>0
replace inv_mun_PT = . if mun_PRD_PT_PC>0
replace inv_mun_PC = . if mun_PRD_PT_PC>0
replace inv_mun_PRD = . if mun_PRD_PC>0
replace inv_mun_PC = . if mun_PRD_PC>0
replace inv_mun_PVEM = . if mun_PVEM_PANAL>0
replace inv_mun_PANAL = . if mun_PVEM_PANAL>0
replace inv_mun_PRI = . if mun_PRI_PVEM>0
replace inv_mun_PVEM = . if mun_PRI_PVEM>0

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PC inv_mun_PVEM inv_mun_PANAL inv_mun_PSD inv_mun_PRI_PANAL inv_mun_PRD_PT_PC inv_mun_PRD_PC inv_mun_PVEM_PANAL inv_mun_PRI_PVEM, gen(PAN_r PRI_r PRD_r PT_r PC_r PVEM_r PANAL_r PSD_r PRI_PANAL_r PRD_PT_PC_r PRD_PC_r PVEM_PANAL_r PRI_PVEM_r)
drop inv_mun_*


gen winner = ""
gen second = ""
gen third = ""

foreach var in PAN PRI PRD PT PC PVEM PANAL PSD PRI_PANAL PRD_PT_PC PRD_PC PVEM_PANAL PRI_PVEM {

replace winner = "`var'" if `var'_r == 1
replace second = "`var'" if `var'_r == 2
replace third = "`var'" if `var'_r == 3

}

drop *_r

gen year = 2012
gen month ="July"

sort section

save Morelos_Section_2012.dta, replace

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

clear all
set mem 1g

use Morelos_Section_1997.dta
append using Morelos_Section_2000.dta
append using Morelos_Section_2003.dta
append using Morelos_Section_2006.dta
append using Morelos_Section_2009.dta
append using Morelos_Section_2012.dta

erase Morelos_Section_1997.dta
erase Morelos_Section_2000.dta
erase Morelos_Section_2003.dta
erase Morelos_Section_2006.dta
erase Morelos_Section_2009.dta
erase Morelos_Section_2012.dta

tab winner, missing

gen PAN_winner =0
replace PAN_winner =1 if strpos(winner, "PAN")>0 &  (strpos(winner, "PAN")!=strpos(winner, "PANAL"))
gen winner_counter = PAN_winner

foreach var in PRI PRD PT PC PCD PVEM PANAL PD PSD PAS PSN PMP  {
gen `var'_winner =0
replace `var'_winner =1 if strpos(winner, "`var'")>0 
replace winner_counter = winner_counter + `var'_winner
}

tab winner_counter
count if winner_counter==0

save Morelos_ALL.dta, replace

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct"
capture cd "C:\Users\hlarreguy\Dropbox\Audits\Election Data"
capture cd "/Users/LauT/Dropbox/Trabajos/Incumbency Advantage/Precinct"

save Morelos_ALL.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
