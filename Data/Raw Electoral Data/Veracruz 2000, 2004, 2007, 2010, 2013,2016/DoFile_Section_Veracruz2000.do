
clear all
set mem 1g

capture cd "D:\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Veracruz 2000, 2004, 2007, 2010, 2013"
capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Data Analysis\Raw Data\Precinct\Veracruz 2000, 2004, 2007, 2010, 2013"

**************************************************************************
**************************************************************************
**************************************************************************

use using  Ayu_2000.dta, clear

gen weight_cc1 = 0
gen weight_cc2 = 0
gen weight_cc3 = 0

*** CC1 ***
replace weight_cc1 = 1 if cc1 != "" & cc2=="" & cc3==""

*** CC1 & CC2 ***

replace weight_cc1 = cdppn_pcd /(cdppn_pcd + pri_dsppn) if cc1 == "CDPPN-PCD" & cc2=="PRI-DSPPN" & cc3==""
replace weight_cc2 = pri_dsppn /(cdppn_pcd + pri_dsppn) if cc1 == "CDPPN-PCD" & cc2=="PRI-DSPPN" & cc3==""

replace weight_cc1 =  pan_cdppn/(pan_cdppn + parm + pri_parm_dsppn) if cc1 == "PAN-CDPPN" & cc2=="PRI-PARM-DSPPN" & cc3==""
replace weight_cc2 =  (parm + pri_parm_dsppn) /(pan_cdppn + parm + pri_parm_dsppn) if cc1 == "PAN-CDPPN" & cc2=="PRI-PARM-DSPPN" & cc3==""

replace weight_cc1 =  pan_cdppn/(pan_cdppn + parm +  psn + pri_parm_psn_dsppn) if cc1 == "PAN-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""
replace weight_cc2 =  (parm + psn + pri_parm_psn_dsppn) /(pan_cdppn + parm + psn + pri_parm_psn_dsppn) if cc1 == "PAN-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""

replace weight_cc1 =  pan_prd/( pan_prd + parm + psn + pri_parm_psn) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN" & cc3==""
replace weight_cc2 =  (parm + psn + pri_parm_psn) /( pan_prd + parm + psn + pri_parm_psn ) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN" & cc3==""

replace weight_cc1 =  pan_prd/( pan_prd + parm + psn + pri_parm_psn_dsppn) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""
replace weight_cc2 =  (parm + psn + pri_parm_psn_dsppn) /( pan_prd + parm + psn + pri_parm_psn_dsppn ) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""

replace weight_cc1 =  pan_prd/( pan_prd + parm + psn + pas + pri_parm_psn_pas) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-PAS" & cc3==""
replace weight_cc2 =  (parm + psn + pas  + pri_parm_psn_pas) /( pan_prd + parm + psn + pas + pri_parm_psn_pas ) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-PAS" & cc3==""

replace weight_cc1 =  pan_prd/( pan_prd + psn + pri_psn) if cc1 == "PAN-PRD" & cc2=="PRI-PSN" & cc3==""
replace weight_cc2 =  (psn + pri_psn) /( pan_prd + psn + pri_psn ) if cc1 == "PAN-PRD" & cc2=="PRI-PSN" & cc3==""

replace weight_cc1 = (pan_prd_cdppn)/((pan_prd_cdppn)+(parm + psn + pas + pri_parm_psn_dsppn_pas)) if cc1 == "PAN-PRD-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN-PAS" & cc3==""
replace weight_cc2 = (parm + psn + pas + pri_parm_psn_dsppn_pas)/((pan_prd_cdppn)+(parm + psn + pas + pri_parm_psn_dsppn_pas)) if cc1 == "PAN-PRD-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN-PAS" & cc3==""

replace weight_cc1 = (pan_pt)/((pan_pt)+(parm + psn + pri_parm_psn)) if cc1 == "PAN-PT" & cc2=="PRI-PARM-PSN" & cc3==""
replace weight_cc2 = (parm + psn + pri_parm_psn)/((pan_pt)+(parm + psn + pri_parm_psn)) if cc1 == "PAN-PT" & cc2=="PRI-PARM-PSN" & cc3==""

replace weight_cc1 = (pan_pvem)/((pan_pvem)+(pri_dsppn)) if cc1 == "PAN-PVEM" & cc2=="PRI-DSPPN" & cc3==""
replace weight_cc2 = (pri_dsppn)/((pan_pvem)+(pri_dsppn)) if cc1 == "PAN-PVEM" & cc2=="PRI-DSPPN" & cc3==""

replace weight_cc1 = (pas + pas_pvem)/((pas + pas_pvem)+(parm + psn +  pri_parm_psn_dsppn)) if cc1 == "PAS-PVEM" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""
replace weight_cc2 = (parm + psn +  pri_parm_psn_dsppn)/((pas + pas_pvem)+(parm + psn +  pri_parm_psn_dsppn)) if cc1 == "PAS-PVEM" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""

replace weight_cc1 = (prd_cdppn)/((prd_cdppn)+(parm + psn +  pri_parm_psn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PARM-PSN" & cc3==""
replace weight_cc2 = (parm + psn +  pri_parm_psn)/((prd_cdppn)+(parm + psn +  pri_parm_psn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PARM-PSN" & cc3==""

replace weight_cc1 = (prd_cdppn)/((prd_cdppn)+(psn + pri_psn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PSN" & cc3==""
replace weight_cc2 = (psn + pri_psn)/((prd_cdppn)+(psn + pri_psn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PSN" & cc3==""

replace weight_cc1 = (prd_cdppn)/((prd_cdppn)+(psn + pri_psn_dsppn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PSN-DSPPN" & cc3==""
replace weight_cc2 = (psn + pri_psn_dsppn)/((prd_cdppn)+(psn + pri_psn_dsppn)) if cc1 == "PRD-CDPPN" & cc2=="PRI-PSN-DSPPN" & cc3==""

replace weight_cc1 = (prd_cdppn)/((prd_cdppn)+(pt_dsppn)) if cc1 == "PRD-CDPPN" & cc2=="PT-DSPPN" & cc3==""
replace weight_cc2 = (pt_dsppn)/((prd_cdppn)+(pt_dsppn)) if cc1 == "PRD-CDPPN" & cc2=="PT-DSPPN" & cc3==""

replace weight_cc1 = (parm + prd_parm_pcd)/((parm + prd_parm_pcd)+(pas + pt_pvem_cdppn_pas_dsppn)) if cc1 == "PRD-PARM-PCD" & cc2=="PT-PVEM-CDPPN-PAS-DSPPN" & cc3==""
replace weight_cc2 = (pas + pt_pvem_cdppn_pas_dsppn)/((parm + prd_parm_pcd)+(pas + pt_pvem_cdppn_pas_dsppn)) if cc1 == "PRD-PARM-PCD" & cc2=="PT-PVEM-CDPPN-PAS-DSPPN" & cc3==""

replace weight_cc1 = (prd_pt)/((prd_pt)+(pas + psn + pri_psn_dsppn_pas)) if cc1 == "PRD-PT" & cc2=="PRI-PSN-DSPPN-PAS" & cc3==""
replace weight_cc2 = (pas + psn + pri_psn_dsppn_pas)/((prd_pt)+(pas + psn + pri_psn_dsppn_pas)) if cc1 == "PRD-PT" & cc2=="PRI-PSN-DSPPN-PAS" & cc3==""

replace weight_cc1 = (prd_pt_cdppn)/((prd_pt_cdppn)+(psn + pri_psn_dsppn)) if cc1 == "PRD-PT-CDPPN" & cc2=="PRI-PSN-DSPPN" & cc3==""
replace weight_cc2 = (psn + pri_psn_dsppn)/((prd_pt_cdppn)+(psn + pri_psn_dsppn)) if cc1 == "PRD-PT-CDPPN" & cc2=="PRI-PSN-DSPPN" & cc3==""

replace weight_cc1 = (prd_pt_pvem_cdppn)/((prd_pt_pvem_cdppn)+(parm + psn + pas + pri_parm_psn_dsppn_pas)) if cc1 == "PRD-PT-PVEM-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN-PAS" & cc3==""
replace weight_cc2 = (parm + psn + pas + pri_parm_psn_dsppn_pas)/((prd_pt_pvem_cdppn)+(parm + psn + pas + pri_parm_psn_dsppn_pas)) if cc1 == "PRD-PT-PVEM-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN-PAS" & cc3==""

replace weight_cc1 = (prd_pvem)/((prd_pvem)+(pt_cdppn)) if cc1 == "PRD-PVEM" & cc2=="PT-CDPPN" & cc3==""
replace weight_cc2 = (pt_cdppn)/((prd_pvem)+(pt_cdppn)) if cc1 == "PRD-PVEM" & cc2=="PT-CDPPN" & cc3==""

replace weight_cc1 = (pas + prd_pvem_cdppn_pas)/((pas + prd_pvem_cdppn_pas)+(psn +  pri_psn_dsppn)) if cc1 == "PRD-PVEM-CDPPN-PAS" & cc2=="PRI-PSN-DSPPN" & cc3==""
replace weight_cc2 = (psn +  pri_psn_dsppn)/((pas + prd_pvem_cdppn_pas)+(psn +  pri_psn_dsppn)) if cc1 == "PRD-PVEM-CDPPN-PAS" & cc2=="PRI-PSN-DSPPN" & cc3==""

replace weight_cc1 = (pri_dsppn)/((pri_dsppn)+(prd_pvem)) if cc1 == "PRI-DSPPN" & cc2=="PRD-PVEM" & cc3==""
replace weight_cc2 = (prd_pvem)/((pri_dsppn)+(prd_pvem)) if cc1 == "PRI-DSPPN" & cc2=="PRD-PVEM" & cc3==""

replace weight_cc1 = (pri_dsppn)/((pri_dsppn)+(parm + pt_cdppn_parm)) if cc1 == "PRI-DSPPN" & cc2=="PT-CDPPN-PARM" & cc3==""
replace weight_cc2 = (parm + pt_cdppn_parm)/((pri_dsppn)+(parm + pt_cdppn_parm)) if cc1 == "PRI-DSPPN" & cc2=="PT-CDPPN-PARM" & cc3==""

replace weight_cc1 = (pri_dsppn)/((pri_dsppn)+(psn +  pvem_psn)) if cc1 == "PRI-DSPPN" & cc2=="PVEM-PSN" & cc3==""
replace weight_cc2 = (psn +  pvem_psn)/((pri_dsppn)+(psn +  pvem_psn)) if cc1 == "PRI-DSPPN" & cc2=="PVEM-PSN" & cc3==""

replace weight_cc1 = (parm + pri_parm)/((parm + pri_parm)+(pan_pt)) if cc1 == "PRI-PARM" & cc2=="PAN-PT" & cc3==""
replace weight_cc2 = (pan_pt)/((parm + pri_parm)+(pan_pt)) if cc1 == "PRI-PARM" & cc2=="PAN-PT" & cc3==""

replace weight_cc1 = (parm + pri_parm)/((parm + pri_parm)+(pan_pvem)) if cc1 == "PRI-PARM" & cc2=="PAN-PVEM" & cc3==""
replace weight_cc2 = (pan_pvem)/((parm + pri_parm)+(pan_pvem)) if cc1 == "PRI-PARM" & cc2=="PAN-PVEM" & cc3==""

replace weight_cc1 = (parm + pas + pri_parm_dsppn_pas)/((parm + pas + pri_parm_dsppn_pas)+(psn + cdppn_psn)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="CDPPN-PSN" & cc3==""
replace weight_cc2 = (psn + cdppn_psn)/((parm + pas + pri_parm_dsppn_pas)+(psn + cdppn_psn)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="CDPPN-PSN" & cc3==""

replace weight_cc1 = (parm + pas + pri_parm_dsppn_pas)/((parm + pas + pri_parm_dsppn_pas)+(pan_prd)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="PAN-PRD" & cc3==""
replace weight_cc2 = (pan_prd)/((parm + pas + pri_parm_dsppn_pas)+(pan_prd)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="PAN-PRD" & cc3==""

replace weight_cc1 = (parm + pas + pri_parm_pas)/((parm + pas + pri_parm_pas)+(pt_cdppn)) if cc1 == "PRI-PARM-PAS" & cc2=="PT-CDPPN" & cc3==""
replace weight_cc2 = (pt_cdppn)/((parm + pas + pri_parm_pas)+(pt_cdppn)) if cc1 == "PRI-PARM-PAS" & cc2=="PT-CDPPN" & cc3==""

replace weight_cc1 = (parm + pas + pri_parm_pas)/((parm + pas + pri_parm_pas)+(pt_pvem_cdppn)) if cc1 == "PRI-PARM-PAS" & cc2=="PT-PVEM-CDPPN" & cc3==""
replace weight_cc2 = (pt_pvem_cdppn)/((parm + pas + pri_parm_pas)+(pt_pvem_cdppn)) if cc1 == "PRI-PARM-PAS" & cc2=="PT-PVEM-CDPPN" & cc3==""

replace weight_cc1 = (parm + psn + pri_parm_psn)/((parm + psn + pri_parm_psn)+(pan_pvem)) if cc1 == "PRI-PARM-PSN" & cc2=="PAN-PVEM" & cc3==""
replace weight_cc2 = (pan_pvem)/((parm + psn + pri_parm_psn)+(pan_pvem)) if cc1 == "PRI-PARM-PSN" & cc2=="PAN-PVEM" & cc3==""

replace weight_cc1 = (parm + psn + pri_parm_psn)/((parm + psn + pri_parm_psn)+(prd_pt)) if cc1 == "PRI-PARM-PSN" & cc2=="PRD-PT" & cc3==""
replace weight_cc2 = (prd_pt)/((parm + psn + pri_parm_psn)+(prd_pt)) if cc1 == "PRI-PARM-PSN" & cc2=="PRD-PT" & cc3==""

replace weight_cc1 = (parm + psn + pri_parm_psn_dsppn)/((parm + psn + pri_parm_psn_dsppn)+(pan_pvem)) if cc1 == "PRI-PARM-PSN-DSPPN" & cc2=="PAN-PVEM" & cc3==""
replace weight_cc2 = (pan_pvem)/((parm + psn + pri_parm_psn_dsppn)+(pan_pvem)) if cc1 == "PRI-PARM-PSN-DSPPN" & cc2=="PAN-PVEM" & cc3==""

replace weight_cc1 = (parm + psn + pri_parm_psn_dsppn)/((parm + psn + pri_parm_psn_dsppn)+(prd_cdppn)) if cc1 == "PRI-PARM-PSN-DSPPN" & cc2=="PRD-CDPPN" & cc3==""
replace weight_cc2 = (prd_cdppn)/((parm + psn + pri_parm_psn_dsppn)+(prd_cdppn)) if cc1 == "PRI-PARM-PSN-DSPPN" & cc2=="PRD-CDPPN" & cc3==""

replace weight_cc1 = (parm + psn + pas + pri_parm_psn_pas)/((parm + psn + pas + pri_parm_psn_pas)+(pan_prd)) if cc1 == "PRI-PARM-PSN-PAS" & cc2=="PAN-PRD" & cc3==""
replace weight_cc2 = (pan_prd)/((parm + psn + pas + pri_parm_psn_pas)+(pan_prd)) if cc1 == "PRI-PARM-PSN-PAS" & cc2=="PAN-PRD" & cc3==""

replace weight_cc1 = (pas + pri_pas)/((pas + pri_pas)+(pan_prd)) if cc1 == "PRI-PAS" & cc2=="PAN-PRD" & cc3==""
replace weight_cc2 = (pan_prd)/((pas + pri_pas)+(pan_prd)) if cc1 == "PRI-PAS" & cc2=="PAN-PRD" & cc3==""

replace weight_cc1 = (psn + pri_psn)/((psn + pri_psn)+(pt_cdppn)) if cc1 == "PRI-PSN" & cc2=="PT-CDPPN" & cc3==""
replace weight_cc2 = (pt_cdppn)/((psn + pri_psn)+(pt_cdppn)) if cc1 == "PRI-PSN" & cc2=="PT-CDPPN" & cc3==""

replace weight_cc1 = (pt_cdppn)/((pt_cdppn)+(pri_dsppn)) if cc1 == "PT-CDPPN" & cc2=="PRI-DSPPN" & cc3==""
replace weight_cc2 = (pri_dsppn)/((pt_cdppn)+(pri_dsppn)) if cc1 == "PT-CDPPN" & cc2=="PRI-DSPPN" & cc3==""

replace weight_cc1 = (pt_cdppn)/((pt_cdppn)+(parm + pri_parm)) if cc1 == "PT-CDPPN" & cc2=="PRI-PARM" & cc3==""
replace weight_cc2 = (parm + pri_parm)/((pt_cdppn)+(parm + pri_parm)) if cc1 == "PT-CDPPN" & cc2=="PRI-PARM" & cc3==""

replace weight_cc1 = (pt_cdppn)/((pt_cdppn)+(parm + psn + pri_parm_psn_dsppn)) if cc1 == "PT-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""
replace weight_cc2 = (parm + psn + pri_parm_psn_dsppn)/((pt_cdppn)+(parm + psn + pri_parm_psn_dsppn)) if cc1 == "PT-CDPPN" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""

replace weight_cc1 = (pt_cdppn)/((pt_cdppn)+(psn +  pri_psn)) if cc1 == "PT-CDPPN" & cc2=="PRI-PSN" & cc3==""
replace weight_cc2 = (psn +  pri_psn)/((pt_cdppn)+(psn +  pri_psn)) if cc1 == "PT-CDPPN" & cc2=="PRI-PSN" & cc3==""

replace weight_cc1 = (pt_pvem)/((pt_pvem)+(parm + psn +  pri_parm_psn)) if cc1 == "PT-PVEM" & cc2=="PRI-PARM-PSN" & cc3==""
replace weight_cc2 = (parm + psn +  pri_parm_psn)/((pt_pvem)+(parm + psn +  pri_parm_psn)) if cc1 == "PT-PVEM" & cc2=="PRI-PARM-PSN" & cc3==""

replace weight_cc1 = (pt_pvem)/((pt_pvem)+(parm + psn + pri_parm_psn_dsppn)) if cc1 == "PT-PVEM" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""
replace weight_cc2 = (parm + psn + pri_parm_psn_dsppn)/((pt_pvem)+(parm + psn + pri_parm_psn_dsppn)) if cc1 == "PT-PVEM" & cc2=="PRI-PARM-PSN-DSPPN" & cc3==""

replace weight_cc1 = (pt_pvem)/((pt_pvem)+(psn +  pri_psn_dsppn)) if cc1 == "PT-PVEM" & cc2=="PRI-PSN-DSPPN" & cc3==""
replace weight_cc2 = (psn +  pri_psn_dsppn)/((pt_pvem)+(psn +  pri_psn_dsppn)) if cc1 == "PT-PVEM" & cc2=="PRI-PSN-DSPPN" & cc3==""

replace weight_cc1 = (pas + pt_pvem_pas)/((pas + pt_pvem_pas)+(parm + psn + prd_cdppn_psn_parm)) if cc1 == "PT-PVEM-PAS" & cc2=="PRD-CDPPN-PSN-PARM" & cc3==""
replace weight_cc2 = (parm + psn + prd_cdppn_psn_parm)/((pas + pt_pvem_pas)+(parm + psn + prd_cdppn_psn_parm)) if cc1 == "PT-PVEM-PAS" & cc2=="PRD-CDPPN-PSN-PARM" & cc3==""

replace weight_cc1 = (pvem_cdppn)/((pvem_cdppn)+(parm + pas + parm_pas)) if cc1 == "PVEM-CDPPN" & cc2=="PARM-PAS" & cc3==""
replace weight_cc2 = (parm + pas + parm_pas)/((pvem_cdppn)+(parm + pas + parm_pas)) if cc1 == "PVEM-CDPPN" & cc2=="PARM-PAS" & cc3==""

*** CC1 & CC2 & CC3 ***

replace weight_cc1 = (pas + pas_cdppn)/ ((pas + pas_cdppn) + (pan_dsppn) + (prd_pt_pvem_pcd))  if cc1 == "PAS-CDPPN" & cc2=="PAN-DSPPN" & cc3=="PRD-PT-PVEM-PCD"
replace weight_cc2 = (pan_dsppn)/ ((pas + pas_cdppn) + (pan_dsppn) + (prd_pt_pvem_pcd)) if cc1 == "PAS-CDPPN" & cc2=="PAN-DSPPN" & cc3=="PRD-PT-PVEM-PCD"
replace weight_cc3 = (prd_pt_pvem_pcd)/ ((pas + pas_cdppn) + (pan_dsppn) + (prd_pt_pvem_pcd)) if cc1 == "PAS-CDPPN" & cc2=="PAN-DSPPN" & cc3=="PRD-PT-PVEM-PCD"

replace weight_cc1 =  (parm + pas + pri_parm_dsppn_pas) / ((parm + pas + pri_parm_dsppn_pas) + (pan_prd) + (pt_cdppn)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="PAN-PRD" & cc3=="PT-CDPPN"
replace weight_cc2 =  (pan_prd)/ ((parm + pas + pri_parm_dsppn_pas) + (pan_prd) + (pt_cdppn)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="PAN-PRD" & cc3=="PT-CDPPN"
replace weight_cc3 =  (pt_cdppn)/ ((parm + pas + pri_parm_dsppn_pas) + (pan_prd) + (pt_cdppn)) if cc1 == "PRI-PARM-DSPPN-PAS" & cc2=="PAN-PRD" & cc3=="PT-CDPPN"

replace weight_cc1 = (psn + pas + pri_psn_pas)/ ((psn + pas + pri_psn_pas) + (pan_pvem) + (parm + parm_dsppn)) if cc1 == "PRI-PSN-PAS" & cc2=="PAN-PVEM" & cc3=="PARM-DSPPN"
replace weight_cc2 = (pan_pvem)/ ((psn + pas + pri_psn_pas) + (pan_pvem) + (parm + parm_dsppn))  if cc1 == "PRI-PSN-PAS" & cc2=="PAN-PVEM" & cc3=="PARM-DSPPN"
replace weight_cc3 = (parm + parm_dsppn)/ ((psn + pas + pri_psn_pas) + (pan_pvem) + (parm + parm_dsppn))  if cc1 == "PRI-PSN-PAS" & cc2=="PAN-PVEM" & cc3=="PARM-DSPPN"

replace weight_cc1 =  (pan_prd)/ ((pan_prd) + (parm + psn + pri_parm_psn_dsppn) + (pt_cdppn)) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-DSPPN" & cc3=="PT-CDPPN"
replace weight_cc2 =  (parm + psn + pri_parm_psn_dsppn)/ ((pan_prd) + (parm + psn + pri_parm_psn_dsppn) + (pt_cdppn))  if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-DSPPN" & cc3=="PT-CDPPN"
replace weight_cc3 =  (pt_cdppn)/ ((pan_prd) + (parm + psn + pri_parm_psn_dsppn) + (pt_cdppn)) if cc1 == "PAN-PRD" & cc2=="PRI-PARM-PSN-DSPPN" & cc3=="PT-CDPPN"

replace weight_cc1 = (parm + pri_parm)/ ((parm + pri_parm) + (pan_pvem) + (pt_cdppn))  if cc1 == "PRI-PARM" & cc2=="PAN-PVEM" & cc3=="PT-CDPPN"
replace weight_cc2 = (pan_pvem)/ ((parm + pri_parm) + (pan_pvem) + (pt_cdppn)) if cc1 == "PRI-PARM" & cc2=="PAN-PVEM" & cc3=="PT-CDPPN"
replace weight_cc3 = (pt_cdppn)/ ((parm + pri_parm) + (pan_pvem) + (pt_cdppn))  if cc1 == "PRI-PARM" & cc2=="PAN-PVEM" & cc3=="PT-CDPPN"

keep  municipality cc1 cc2 cc3 coalition_alianza weight_cc1 weight_cc2 weight_cc3

sort municipality
save Ayu_2000_Weights.dta, replace

*************************************************************************
*************************************************************************
*************************************************************************

insheet using  Ayu_Seccion_2000_No_LN.csv, clear

rename municipio  municipality
rename seccion section
* rename listanominal nominal

replace municipality ="MARTINEZ DE LA TORRE" if municipality=="SAN RAFAEL"
replace municipality ="PLAYA VICENTE" if municipality=="SANTIAGO SOCHIAPA"

drop if municipality=="" & section==.
drop if total==. | total==0 

destring pan - total , replace

collapse (sum)  pan - total , by (municipality section)

sort municipality
merge municipality using  Ayu_2000_Weights.dta
drop if _merge==2
drop _merge

* AYAHUALULCO IXHUATLANCILLO 
* They are all zeros in the electoral precinct data
* and somehow they were not dropped above

*************************************************************************

gen pan_prd = pan + prd + alianza if coalition_alianza =="PAN-PRD"
replace pan = 0 if coalition_alianza =="PAN-PRD"
replace prd = 0 if coalition_alianza =="PAN-PRD"
replace alianza = 0 if coalition_alianza =="PAN-PRD"

gen cdppn_pcd=0
gen cdppn_psn=0
gen pan_cdppn=0
gen pan_dsppn=0
gen pan_prd_cdppn=0
gen pan_prd_pvem=0
gen pan_pt=0
gen pan_pvem=0
gen parm_dsppn=0
gen parm_dsppn_pas=0
gen parm_pas=0
gen pas_cdppn=0
gen pas_pvem=0
gen prd_cdppn=0
gen prd_cdppn_psn_parm=0
gen prd_parm_pcd=0
gen prd_pt=0
gen prd_pt_cdppn=0
gen prd_pt_cdppn_pas=0
gen prd_pt_pvem_cdppn=0
gen prd_pt_pvem_pcd=0
gen prd_pvem=0
gen prd_pvem_cdppn_pas=0
gen pri_dsppn=0
gen pri_parm=0
gen pri_parm_dsppn=0
gen pri_parm_dsppn_pas=0
gen pri_parm_pas=0
gen pri_parm_psn=0
gen pri_parm_psn_dsppn=0
gen pri_parm_psn_dsppn_pas=0
gen pri_parm_psn_pas=0
gen pri_pas=0
gen pri_psn=0
gen pri_psn_dsppn=0
gen pri_psn_dsppn_pas=0
gen pri_psn_pas=0
gen pt_cdppn=0
gen pt_cdppn_parm=0
gen pt_cdppn_psn=0
gen pt_dsppn=0
gen pt_parm=0
gen pt_pvem=0
gen pt_pvem_cdppn=0
gen pt_pvem_cdppn_pas_dsppn=0
gen pt_pvem_parm_dsppn=0
gen pt_pvem_pas=0
gen pvem_cdppn=0
gen pvem_parm_cdppn=0
gen pvem_psn=0

**** CC1 ****
replace cdppn_pcd=cdppn + pcd + totaldecc * weight_cc1 if cc1=="CDPPN-PCD"
replace pan_cdppn=pan + cdppn + totaldecc * weight_cc1 if cc1=="PAN-CDPPN"
replace pan_prd=pan + prd + totaldecc * weight_cc1 if cc1=="PAN-PRD"
replace pan_prd_cdppn=pan + prd + cdppn + totaldecc * weight_cc1 if cc1=="PAN-PRD-CDPPN"
replace pan_prd_pvem=pan + prd + pvem + totaldecc * weight_cc1 if cc1=="PAN-PRD-PVEM"
replace pan_pt=pan + pt + totaldecc * weight_cc1 if cc1=="PAN-PT"
replace pan_pvem=pan + pvem + totaldecc * weight_cc1 if cc1=="PAN-PVEM"
replace parm_dsppn_pas=dsppn  + totaldecc * weight_cc1 if cc1=="PARM-DSPPN-PAS"
replace parm_pas=  totaldecc * weight_cc1 if cc1=="PARM-PAS"
replace pas_cdppn=cdppn + totaldecc * weight_cc1 if cc1=="PAS-CDPPN"
replace pas_pvem=pvem + totaldecc * weight_cc1 if cc1=="PAS-PVEM"
replace prd_cdppn=prd + cdppn + totaldecc * weight_cc1 if cc1=="PRD-CDPPN"
replace prd_parm_pcd=prd + pcd + totaldecc * weight_cc1 if cc1=="PRD-PARM-PCD"
replace prd_pt=prd + pt + totaldecc * weight_cc1 if cc1=="PRD-PT"
replace prd_pt_cdppn=prd + pt + cdppn + totaldecc * weight_cc1 if cc1=="PRD-PT-CDPPN"
replace prd_pt_cdppn_pas=prd + pt + cdppn  + totaldecc * weight_cc1 if cc1=="PRD-PT-CDPPN-PAS"
replace prd_pt_pvem_cdppn=prd + pt + pvem + cdppn + totaldecc * weight_cc1 if cc1=="PRD-PT-PVEM-CDPPN"
replace prd_pvem=prd + pvem + totaldecc * weight_cc1 if cc1=="PRD-PVEM"
replace prd_pvem_cdppn_pas=prd + pvem + cdppn  + totaldecc * weight_cc1 if cc1=="PRD-PVEM-CDPPN-PAS"
replace pri_dsppn=pri + dsppn + totaldecc * weight_cc1 if cc1=="PRI-DSPPN"
replace pri_dsppn=pri + dsppn + totaldecc * weight_cc1 if cc1=="PRI-DSPPN"
replace pri_parm=pri  + totaldecc * weight_cc1 if cc1=="PRI-PARM"
replace pri_parm_dsppn=pri +  dsppn + totaldecc * weight_cc1 if cc1=="PRI-PARM-DSPPN"
replace pri_parm_dsppn_pas=pri +  dsppn  + totaldecc * weight_cc1 if cc1=="PRI-PARM-DSPPN-PAS"
replace pri_parm_pas=pri + totaldecc * weight_cc1 if cc1=="PRI-PARM-PAS"
replace pri_parm_psn=pri + totaldecc * weight_cc1 if cc1=="PRI-PARM-PSN"
replace pri_parm_psn_dsppn=pri + dsppn + totaldecc * weight_cc1 if cc1=="PRI-PARM-PSN-DSPPN"
replace pri_parm_psn_dsppn_pas=pri + dsppn  + totaldecc * weight_cc1 if cc1=="PRI-PARM-PSN-DSPPN-PAS"
replace pri_parm_psn_pas=pri  + totaldecc * weight_cc1 if cc1=="PRI-PARM-PSN-PAS"
replace pri_pas=pri  + totaldecc * weight_cc1 if cc1=="PRI-PAS"
replace pri_psn=pri + totaldecc * weight_cc1 if cc1=="PRI-PSN"
replace pri_psn_dsppn=pri + dsppn + totaldecc * weight_cc1 if cc1=="PRI-PSN-DSPPN"
replace pri_psn_dsppn_pas=pri + dsppn  + totaldecc * weight_cc1 if cc1=="PRI-PSN-DSPPN-PAS"
replace pri_psn_pas=pri  + totaldecc * weight_cc1 if cc1=="PRI-PSN-PAS"
replace pt_cdppn=pt + cdppn + totaldecc * weight_cc1 if cc1=="PT-CDPPN"
replace pt_cdppn_psn=pt + cdppn + totaldecc * weight_cc1 if cc1=="PT-CDPPN-PSN"
replace pt_parm=pt  + totaldecc * weight_cc1 if cc1=="PT-PARM"
replace pt_pvem=pt + pvem + totaldecc * weight_cc1 if cc1=="PT-PVEM"
replace pt_pvem_parm_dsppn=pt + pvem +  dsppn + totaldecc * weight_cc1 if cc1=="PT-PVEM-PARM-DSPPN"
replace pt_pvem_pas=pt + pvem  + totaldecc * weight_cc1 if cc1=="PT-PVEM-PAS"
replace pvem_cdppn=pvem + cdppn + totaldecc * weight_cc1 if cc1=="PVEM-CDPPN"
replace pvem_parm_cdppn=pvem +  cdppn + totaldecc * weight_cc1 if cc1=="PVEM-PARM-CDPPN"

replace cdppn=0 if cc1=="CDPPN-PCD"
replace pcd=0 if cc1=="CDPPN-PCD"
replace pan=0 if cc1=="PAN-CDPPN"
replace cdppn=0 if cc1=="PAN-CDPPN"
replace pan=0 if cc1=="PAN-PRD"
replace prd=0 if cc1=="PAN-PRD"
replace pan=0 if cc1=="PAN-PRD-CDPPN"
replace prd=0 if cc1=="PAN-PRD-CDPPN"
replace cdppn=0 if cc1=="PAN-PRD-CDPPN"
replace pan=0 if cc1=="PAN-PRD-PVEM"
replace prd=0 if cc1=="PAN-PRD-PVEM"
replace pvem=0 if cc1=="PAN-PRD-PVEM"
replace pan=0 if cc1=="PAN-PT"
replace pt=0 if cc1=="PAN-PT"
replace pan=0 if cc1=="PAN-PVEM"
replace pvem=0 if cc1=="PAN-PVEM"
replace dsppn =0 if cc1=="PARM-DSPPN-PAS"
replace cdppn=0 if cc1=="PAS-CDPPN"
replace pvem=0 if cc1=="PAS-PVEM"
replace prd=0 if cc1=="PRD-CDPPN"
replace cdppn=0 if cc1=="PRD-CDPPN"
replace prd=0 if cc1=="PRD-PARM-PCD"
replace pcd=0 if cc1=="PRD-PARM-PCD"
replace prd=0 if cc1=="PRD-PT"
replace pt=0 if cc1=="PRD-PT"
replace prd=0 if cc1=="PRD-PT-CDPPN"
replace pt=0 if cc1=="PRD-PT-CDPPN"
replace cdppn=0 if cc1=="PRD-PT-CDPPN"
replace prd=0 if cc1=="PRD-PT-CDPPN-PAS"
replace pt=0 if cc1=="PRD-PT-CDPPN-PAS"
replace cdppn=0 if cc1=="PRD-PT-CDPPN-PAS"
replace prd=0 if cc1=="PRD-PT-PVEM-CDPPN"
replace pt=0 if cc1=="PRD-PT-PVEM-CDPPN"
replace pvem=0 if cc1=="PRD-PT-PVEM-CDPPN"
replace cdppn=0 if cc1=="PRD-PT-PVEM-CDPPN"
replace prd=0 if cc1=="PRD-PVEM"
replace pvem=0 if cc1=="PRD-PVEM"
replace prd=0 if cc1=="PRD-PVEM-CDPPN-PAS"
replace pvem=0 if cc1=="PRD-PVEM-CDPPN-PAS"
replace cdppn =0 if cc1=="PRD-PVEM-CDPPN-PAS"
replace pri=0 if cc1=="PRI-DSPPN"
replace dsppn=0 if cc1=="PRI-DSPPN"
replace pri=0 if cc1=="PRI-DSPPN"
replace dsppn=0 if cc1=="PRI-DSPPN"
replace pri =0 if cc1=="PRI-PARM"
replace pri=0 if cc1=="PRI-PARM-DSPPN"
replace dsppn=0 if cc1=="PRI-PARM-DSPPN"
replace pri=0 if cc1=="PRI-PARM-DSPPN-PAS"
replace dsppn=0 if cc1=="PRI-PARM-DSPPN-PAS"
replace pri=0 if cc1=="PRI-PARM-PAS"
replace pri=0 if cc1=="PRI-PARM-PSN"
replace pri=0 if cc1=="PRI-PARM-PSN-DSPPN"
replace dsppn =0 if cc1=="PRI-PARM-PSN-DSPPN"
replace pri=0 if cc1=="PRI-PARM-PSN-DSPPN-PAS"
replace dsppn =0 if cc1=="PRI-PARM-PSN-DSPPN-PAS"
replace pri =0 if cc1=="PRI-PARM-PSN-PAS"
replace pri =0 if cc1=="PRI-PAS"
replace pri=0 if cc1=="PRI-PSN"
replace pri=0 if cc1=="PRI-PSN-DSPPN"
replace dsppn=0 if cc1=="PRI-PSN-DSPPN"
replace pri=0 if cc1=="PRI-PSN-DSPPN-PAS"
replace dsppn=0 if cc1=="PRI-PSN-DSPPN-PAS"
replace pri =0 if cc1=="PRI-PSN-PAS"
replace pt=0 if cc1=="PT-CDPPN"
replace cdppn=0 if cc1=="PT-CDPPN"
replace pt=0 if cc1=="PT-CDPPN-PSN"
replace cdppn=0 if cc1=="PT-CDPPN-PSN"
replace pt =0 if cc1=="PT-PARM"
replace pt=0 if cc1=="PT-PVEM"
replace pvem=0 if cc1=="PT-PVEM"
replace pt=0 if cc1=="PT-PVEM-PARM-DSPPN"
replace pvem=0 if cc1=="PT-PVEM-PARM-DSPPN"
replace dsppn=0 if cc1=="PT-PVEM-PARM-DSPPN"
replace pt=0 if cc1=="PT-PVEM-PAS"
replace pvem=0 if cc1=="PT-PVEM-PAS"
replace pvem=0 if cc1=="PVEM-CDPPN"
replace cdppn=0 if cc1=="PVEM-CDPPN"
replace pvem=0 if cc1=="PVEM-PARM-CDPPN"
replace cdppn=0 if cc1=="PVEM-PARM-CDPPN"

**** CC2 ****
replace cdppn_psn=cdppn + totaldecc * weight_cc2 if cc2=="CDPPN-PSN"
replace pan_dsppn=pan + dsppn + totaldecc * weight_cc2 if cc2=="PAN-DSPPN"
replace pan_prd=pan + prd + totaldecc * weight_cc2 if cc2=="PAN-PRD"
replace pan_pt=pan + pt + totaldecc * weight_cc2 if cc2=="PAN-PT"
replace pan_pvem=pan + pvem + totaldecc * weight_cc2 if cc2=="PAN-PVEM"
replace parm_pas= totaldecc * weight_cc2 if cc2=="PARM-PAS"
replace prd_cdppn=prd + cdppn + totaldecc * weight_cc2 if cc2=="PRD-CDPPN"
replace prd_cdppn_psn_parm=prd + cdppn + totaldecc * weight_cc2 if cc2=="PRD-CDPPN-PSN-PARM"
replace prd_pt=prd + pt + totaldecc * weight_cc2 if cc2=="PRD-PT"
replace prd_pvem=prd + pvem + totaldecc * weight_cc2 if cc2=="PRD-PVEM"
replace pri_dsppn=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-DSPPN"
replace pri_parm=pri + totaldecc * weight_cc2 if cc2=="PRI-PARM"
replace pri_parm_dsppn=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-PARM-DSPPN"
replace pri_parm_psn=pri + totaldecc * weight_cc2 if cc2=="PRI-PARM-PSN"
replace pri_parm_psn_dsppn=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-PARM-PSN-DSPPN"
replace pri_parm_psn_dsppn_pas=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-PARM-PSN-DSPPN-PAS"
replace pri_parm_psn_pas=pri + totaldecc * weight_cc2 if cc2=="PRI-PARM-PSN-PAS"
replace pri_psn=pri + totaldecc * weight_cc2 if cc2=="PRI-PSN"
replace pri_psn_dsppn=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-PSN-DSPPN"
replace pri_psn_dsppn_pas=pri + dsppn + totaldecc * weight_cc2 if cc2=="PRI-PSN-DSPPN-PAS"
replace pt_cdppn=pt + cdppn + totaldecc * weight_cc2 if cc2=="PT-CDPPN"
replace pt_cdppn_parm=pt + cdppn + totaldecc * weight_cc2 if cc2=="PT-CDPPN-PARM"
replace pt_dsppn=pt + dsppn + totaldecc * weight_cc2 if cc2=="PT-DSPPN"
replace pt_pvem_cdppn=pt + pvem + cdppn + totaldecc * weight_cc2 if cc2=="PT-PVEM-CDPPN"
replace pt_pvem_cdppn_pas_dsppn=pt + pvem + cdppn + dsppn + totaldecc * weight_cc2 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace pvem_psn=pvem + totaldecc * weight_cc2 if cc2=="PVEM-PSN"

replace cdppn=0 if cc2=="CDPPN-PSN"
replace pan=0 if cc2=="PAN-DSPPN"
replace dsppn=0 if cc2=="PAN-DSPPN"
replace pan=0 if cc2=="PAN-PRD"
replace prd=0 if cc2=="PAN-PRD"
replace pan=0 if cc2=="PAN-PT"
replace pt=0 if cc2=="PAN-PT"
replace pan=0 if cc2=="PAN-PVEM"
replace pvem=0 if cc2=="PAN-PVEM"
replace prd=0 if cc2=="PRD-CDPPN"
replace cdppn=0 if cc2=="PRD-CDPPN"
replace prd=0 if cc2=="PRD-CDPPN-PSN-PARM"
replace cdppn=0 if cc2=="PRD-CDPPN-PSN-PARM"
replace prd=0 if cc2=="PRD-PT"
replace pt=0 if cc2=="PRD-PT"
replace prd=0 if cc2=="PRD-PVEM"
replace pvem=0 if cc2=="PRD-PVEM"
replace pri=0 if cc2=="PRI-DSPPN"
replace pri=0 if cc2=="PRI-DSPPN"
replace dsppn=0 if cc2=="PRI-DSPPN"
replace pri=0 if cc2=="PRI-PARM"
replace pri=0 if cc2=="PRI-PARM-DSPPN"
replace dsppn=0 if cc2=="PRI-PARM-DSPPN"
replace pri=0 if cc2=="PRI-PARM-PSN"
replace pri=0 if cc2=="PRI-PARM-PSN-DSPPN"
replace dsppn=0 if cc2=="PRI-PARM-PSN-DSPPN"
replace pri=0 if cc2=="PRI-PARM-PSN-DSPPN-PAS"
replace dsppn=0 if cc2=="PRI-PARM-PSN-DSPPN-PAS"
replace pri=0 if cc2=="PRI-PARM-PSN-PAS"
replace pri=0 if cc2=="PRI-PSN"
replace pri=0 if cc2=="PRI-PSN-DSPPN"
replace dsppn=0 if cc2=="PRI-PSN-DSPPN"
replace pri=0 if cc2=="PRI-PSN-DSPPN-PAS"
replace dsppn=0 if cc2=="PRI-PSN-DSPPN-PAS"
replace pt=0 if cc2=="PT-CDPPN"
replace cdppn=0 if cc2=="PT-CDPPN"
replace pt=0 if cc2=="PT-CDPPN-PARM"
replace cdppn=0 if cc2=="PT-CDPPN-PARM"
replace pt=0 if cc2=="PT-DSPPN"
replace dsppn=0 if cc2=="PT-DSPPN"
replace pt=0 if cc2=="PT-PVEM-CDPPN"
replace pvem=0 if cc2=="PT-PVEM-CDPPN"
replace cdppn=0 if cc2=="PT-PVEM-CDPPN"
replace pt=0 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace pvem=0 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace cdppn=0 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace dsppn=0 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace pvem=0 if cc2=="PVEM-PSN"

**** CC3 ****
replace parm_dsppn=dsppn + totaldecc * weight_cc3 if cc3=="PARM-DSPPN"
replace prd_pt_pvem_pcd=prd + pt + pvem + pcd + totaldecc * weight_cc3 if cc3=="PRD-PT-PVEM-PCD"
replace pt_cdppn=pt + cdppn + totaldecc * weight_cc3 if cc3=="PT-CDPPN"

replace dsppn=0 if cc3=="PARM-DSPPN"
replace prd  = 0 if cc3=="PRD-PT-PVEM-PCD"
replace pt  = 0 if cc3=="PRD-PT-PVEM-PCD"
replace pvem  = 0 if cc3=="PRD-PT-PVEM-PCD"
replace pcd = 0 if cc3=="PRD-PT-PVEM-PCD"
replace pt  = 0 if cc3=="PT-CDPPN"
replace cdppn = 0 if cc3=="PT-CDPPN"

replace totaldecc=0 if cc1=="CDPPN-PCD"
replace totaldecc=0 if cc1=="PAN-CDPPN"
replace totaldecc=0 if cc1=="PAN-CDPPN"
replace totaldecc=0 if cc1=="PAN-PRD"
replace totaldecc=0 if cc1=="PAN-PRD-CDPPN"
replace totaldecc=0 if cc1=="PAN-PRD-PVEM"
replace totaldecc=0 if cc1=="PAN-PT"
replace totaldecc=0 if cc1=="PAN-PVEM"
replace totaldecc=0 if cc1=="PARM-DSPPN-PAS"
replace totaldecc=0 if cc1=="PARM-PAS"
replace totaldecc=0 if cc1=="PAS-CDPPN"
replace totaldecc=0 if cc1=="PAS-PVEM"
replace totaldecc=0 if cc1=="PRD-CDPPN"
replace totaldecc=0 if cc1=="PRD-PARM-PCD"
replace totaldecc=0 if cc1=="PRD-PT"
replace totaldecc=0 if cc1=="PRD-PT-CDPPN"
replace totaldecc=0 if cc1=="PRD-PT-CDPPN-PAS"
replace totaldecc=0 if cc1=="PRD-PT-PVEM-CDPPN"
replace totaldecc=0 if cc1=="PRD-PVEM"
replace totaldecc=0 if cc1=="PRD-PVEM-CDPPN-PAS"
replace totaldecc=0 if cc1=="PRI-DSPPN"
replace totaldecc=0 if cc1=="PRI-DSPPN"
replace totaldecc=0 if cc1=="PRI-PARM"
replace totaldecc=0 if cc1=="PRI-PARM-DSPPN"
replace totaldecc=0 if cc1=="PRI-PARM-DSPPN-PAS"
replace totaldecc=0 if cc1=="PRI-PARM-PAS"
replace totaldecc=0 if cc1=="PRI-PARM-PSN"
replace totaldecc=0 if cc1=="PRI-PARM-PSN-DSPPN"
replace totaldecc=0 if cc1=="PRI-PARM-PSN-DSPPN-PAS"
replace totaldecc=0 if cc1=="PRI-PARM-PSN-PAS"
replace totaldecc=0 if cc1=="PRI-PAS"
replace totaldecc=0 if cc1=="PRI-PSN"
replace totaldecc=0 if cc1=="PRI-PSN-DSPPN"
replace totaldecc=0 if cc1=="PRI-PSN-DSPPN-PAS"
replace totaldecc=0 if cc1=="PRI-PSN-PAS"
replace totaldecc=0 if cc1=="PT-CDPPN"
replace totaldecc=0 if cc1=="PT-CDPPN-PSN"
replace totaldecc=0 if cc1=="PT-PARM"
replace totaldecc=0 if cc1=="PT-PVEM"
replace totaldecc=0 if cc1=="PT-PVEM-PARM-DSPPN"
replace totaldecc=0 if cc1=="PT-PVEM-PAS"
replace totaldecc=0 if cc1=="PVEM-CDPPN"
replace totaldecc=0 if cc1=="PVEM-PARM-CDPPN"

replace totaldecc=0 if cc2=="PARM-DSPPN"
replace totaldecc=0 if cc2=="PRD-PT-PVEM-PCD"
replace totaldecc=0 if cc2=="PT-CDPPN"
replace totaldecc=0 if cc2=="PAN-PT"
replace totaldecc=0 if cc2=="PAN-PVEM"
replace totaldecc=0 if cc2=="PARM-PAS"
replace totaldecc=0 if cc2=="PRD-CDPPN"
replace totaldecc=0 if cc2=="PRD-CDPPN-PSN-PARM"
replace totaldecc=0 if cc2=="PRD-PT"
replace totaldecc=0 if cc2=="PRD-PVEM"
replace totaldecc=0 if cc2=="PRI-DSPPN"
replace totaldecc=0 if cc2=="PRI-PARM"
replace totaldecc=0 if cc2=="PRI-PARM-DSPPN"
replace totaldecc=0 if cc2=="PRI-PARM-PSN"
replace totaldecc=0 if cc2=="PRI-PARM-PSN-DSPPN"
replace totaldecc=0 if cc2=="PRI-PARM-PSN-DSPPN-PAS"
replace totaldecc=0 if cc2=="PRI-PARM-PSN-PAS"
replace totaldecc=0 if cc2=="PRI-PSN"
replace totaldecc=0 if cc2=="PRI-PSN-DSPPN"
replace totaldecc=0 if cc2=="PRI-PSN-DSPPN-PAS"
replace totaldecc=0 if cc2=="PT-CDPPN"
replace totaldecc=0 if cc2=="PT-CDPPN-PARM"
replace totaldecc=0 if cc2=="PT-DSPPN"
replace totaldecc=0 if cc2=="PT-PVEM-CDPPN"
replace totaldecc=0 if cc2=="PT-PVEM-CDPPN-PAS-DSPPN"
replace totaldecc=0 if cc2=="PVEM-PSN"

replace totaldecc=0 if cc3=="PARM-DSPPN"
replace totaldecc=0 if cc3=="PRD-PT-PVEM-PCD"
replace totaldecc=0 if cc3=="PT-CDPPN"

drop alianza totaldecc cc1 cc2 cc3 coalition_alianza weight_cc1 weight_cc2 weight_cc3

*************************************************************************

rename pan PAN
rename pri PRI
rename prd PRD
rename pt PT
rename pvem PVEM
rename cdppn PC
rename pcd PCD
rename dsppn PDS
rename pan_prd PAN_PRD
rename pt_parm PT_PARM
rename pri_dsppn PRI_PDS
rename pt_cdppn_parm PT_PC_PARM
rename pri_parm_psn PRI_PARM_PSN
rename pt_cdppn PT_PC
rename pri_parm PRI_PARM
rename pri_parm_psn_pas PRI_PARM_PSN_PAS
rename pan_pt PAN_PT
rename pan_cdppn PAN_PC
rename pri_parm_psn_dsppn PRI_PARM_PSN_PDS
rename pan_pvem PAN_PVEM
rename pri_parm_pas PRI_PARM_PAS
rename pri_psn_pas PRI_PSN_PAS
rename pri_parm_psn_dsppn_pas PRI_PARM_PSN_PDS_PAS
rename pri_pas PRI_PAS
rename pan_prd_cdppn PAN_PRD_PC
rename pri_psn PRI_PSN
rename pri_parm_dsppn_pas PRI_PARM_PDS_PAS
rename cdppn_psn PC_PSN
rename parm_dsppn_pas PARM_PDS_PAS
rename pri_parm_dsppn PRI_PARM_PDS
rename pt_pvem_cdppn PT_PVEM_PC
rename prd_pt_pvem_cdppn PRD_PT_PVEM_PC
rename pt_pvem_pas PT_PVEM_PAS
rename prd_cdppn_psn_parm PRD_PC_PSN_PARM
rename pri_psn_dsppn PRI_PSN_PDS
rename pas_pvem PAS_PVEM
rename pvem_parm_cdppn PVEM_PARM_PC
rename pt_pvem PT_PVEM
rename prd_parm_pcd PRD_PARM_PCD
rename pt_pvem_cdppn_pas_dsppn PT_PVEM_PC_PAS_PDS
rename prd_pvem PRD_PVEM
rename parm_dsppn PARM_PDS
rename pri_psn_dsppn_pas PRI_PSN_PDS_PAS
rename prd_cdppn PRD_PC
rename prd_pt PRD_PT
rename pas_cdppn PAS_PC
rename pan_prd_pvem PAN_PRD_PVEM
rename pvem_cdppn PVEM_PC
rename pt_cdppn_psn PT_PC_PSN
rename pt_dsppn PT_PDS
rename prd_pt_cdppn_pas PRD_PT_PC_PAS
rename pt_pvem_parm_dsppn PT_PVEM_PARM_PDS
rename pan_dsppn PAN_PDS
rename prd_pt_pvem_pcd PRD_PT_PVEM_PCD
rename prd_pt_cdppn PRD_PT_PC
rename pvem_psn PVEM_PSN
rename prd_pvem_cdppn_pas PRD_PVEM_PC_PAS
rename parm_pas PARM_PAS
rename cdppn_pcd PC_PCD

* gen turnout =  total/listanominal

drop  nulos  noregistrados  

gen   uniqueid= 0
replace uniqueid=30001 if municipality =="ACAJETE"
replace uniqueid=30002 if municipality =="ACATLAN"
replace uniqueid=30003 if municipality =="ACAYUCAN"
replace uniqueid=30004 if municipality =="ACTOPAN"
replace uniqueid=30005 if municipality =="ACULA"
replace uniqueid=30006 if municipality =="ACULTZINGO"
replace uniqueid=30204 if municipality =="AGUA DULCE"
replace uniqueid=30160 if municipality =="TEMAPACHE"
replace uniqueid=30008 if municipality =="ALPATLAHUAC"
replace uniqueid=30009 if municipality =="ALTO LUCERO"
replace uniqueid=30010 if municipality =="ALTOTONGA"
replace uniqueid=30011 if municipality =="ALVARADO"
replace uniqueid=30012 if municipality =="AMATITLAN"
replace uniqueid=30014 if municipality =="AMATLAN DE LOS REYES"
replace uniqueid=30015 if municipality =="ANGEL R. CABADA"
replace uniqueid=30017 if municipality =="APAZAPAN"
replace uniqueid=30018 if municipality =="AQUILA"
replace uniqueid=30019 if municipality =="ASTACINGA"
replace uniqueid=30020 if municipality =="ATLAHUILCO"
replace uniqueid=30021 if municipality =="ATOYAC"
replace uniqueid=30022 if municipality =="ATZACAN"
replace uniqueid=30023 if municipality =="ATZALAN"
replace uniqueid=30025 if municipality =="AYAHUALULCO"
replace uniqueid=30026 if municipality =="BANDERILLA"
replace uniqueid=30027 if municipality =="BENITO JUAREZ"
replace uniqueid=30028 if municipality =="BOCA DEL RIO"
replace uniqueid=30029 if municipality =="CALCAHUALCO"
replace uniqueid=30007 if municipality =="CAMARON DE TEJEDA"
replace uniqueid=30030 if municipality =="CAMERINO Z. MENDOZA"
replace uniqueid=30208 if municipality =="CARLOS A. CARRILLO"
replace uniqueid=30031 if municipality =="CARRILLO PUERTO"
replace uniqueid=30157 if municipality =="CASTILLO DE TEAYO"
replace uniqueid=30032 if municipality =="CATEMACO"
replace uniqueid=30033 if municipality =="CAZONES DE HERRERA"
replace uniqueid=30034 if municipality =="CERRO AZUL"
replace uniqueid=30054 if municipality =="CHACALTIANGUIS"
replace uniqueid=30055 if municipality =="CHALMA"
replace uniqueid=30056 if municipality =="CHICONAMEL"
replace uniqueid=30057 if municipality =="CHICONQUIACO"
replace uniqueid=30058 if municipality =="CHICONTEPEC"
replace uniqueid=30059 if municipality =="CHINAMECA"
replace uniqueid=30060 if municipality =="CHINAMPA DE GOROSTIZA"
replace uniqueid=30062 if municipality =="CHOCAMAN"
replace uniqueid=30063 if municipality =="CHONTLA"
replace uniqueid=30064 if municipality =="CHUMATLAN"
replace uniqueid=30035 if municipality =="CITLALTEPETL"
replace uniqueid=30036 if municipality =="COACOATZINTLA"
replace uniqueid=30037 if municipality =="COAHUITLAN"
replace uniqueid=30038 if municipality =="COATEPEC"
replace uniqueid=30039 if municipality =="COATZACOALCOS"
replace uniqueid=30040 if municipality =="COATZINTLA"
replace uniqueid=30041 if municipality =="COETZALA"
replace uniqueid=30042 if municipality =="COLIPA"
replace uniqueid=30043 if municipality =="COMAPA"
replace uniqueid=30044 if municipality =="CORDOBA"
replace uniqueid=30045 if municipality =="COSAMALOAPAN"
replace uniqueid=30046 if municipality =="COSAUTLAN DE CARVAJAL"
replace uniqueid=30047 if municipality =="COSCOMATEPEC"
replace uniqueid=30048 if municipality =="COSOLEACAQUE"
replace uniqueid=30049 if municipality =="COTAXTLA"
replace uniqueid=30050 if municipality =="COXQUIHUI"
replace uniqueid=30051 if municipality =="COYUTLA"
replace uniqueid=30052 if municipality =="CUICHAPA"
replace uniqueid=30053 if municipality =="CUITLAHUAC"
replace uniqueid=30205 if municipality =="EL HIGO"
replace uniqueid=30065 if municipality =="EMILIANO ZAPATA"
replace uniqueid=30066 if municipality =="ESPINAL"
replace uniqueid=30067 if municipality =="FILOMENO MATA"
replace uniqueid=30068 if municipality =="FORTIN"
replace uniqueid=30069 if municipality =="GUTIERREZ ZAMORA"
replace uniqueid=30070 if municipality =="HIDALGOTITLAN"
replace uniqueid=30071 if municipality =="HUATUSCO"
replace uniqueid=30072 if municipality =="HUAYACOCOTLA"
replace uniqueid=30073 if municipality =="HUEYAPAN DE OCAMPO"
replace uniqueid=30074 if municipality =="HUILOAPAN DE CUAUHTEMOC"
replace uniqueid=30075 if municipality =="IGNACIO DE LA LLAVE"
replace uniqueid=30076 if municipality =="ILAMATLAN"
replace uniqueid=30077 if municipality =="ISLA"
replace uniqueid=30078 if municipality =="IXCATEPEC"
replace uniqueid=30079 if municipality =="IXHUACAN DE LOS REYES"
replace uniqueid=30083 if municipality =="IXHUATLAN DE MADERO"
replace uniqueid=30080 if municipality =="IXHUATLAN DEL CAFÉ" | strpos(municipality, "IXHUATLAN DEL CAF")>0
replace uniqueid=30082 if municipality =="IXHUATLAN DEL SURESTE"
replace uniqueid=30081 if municipality =="IXHUATLANCILLO"
replace uniqueid=30084 if municipality =="IXMATLAHUACAN"
replace uniqueid=30085 if municipality =="IXTACZOQUITLAN"
replace uniqueid=30086 if municipality =="JALACINGO"
replace uniqueid=30088 if municipality =="JALCOMULCO"
replace uniqueid=30089 if municipality =="JALTIPAN"
replace uniqueid=30090 if municipality =="JAMAPA"
replace uniqueid=30091 if municipality =="JESUS CARRANZA"
replace uniqueid=30093 if municipality =="JILOTEPEC"
replace uniqueid=30169 if municipality =="JOSE AZUETA"
replace uniqueid=30094 if municipality =="JUAN RODRIGUEZ CLARA"
replace uniqueid=30095 if municipality =="JUCHIQUE DE FERRER"
replace uniqueid=30016 if municipality =="LA ANTIGUA"
replace uniqueid=30127 if municipality =="LA PERLA"
replace uniqueid=30096 if municipality =="LANDERO Y COSS"
replace uniqueid=30061 if municipality =="LAS CHOAPAS"
replace uniqueid=30107 if municipality =="LAS MINAS"
replace uniqueid=30132 if municipality =="LAS VIGAS DE RAMIREZ"
replace uniqueid=30097 if municipality =="LERDO DE TEJADA"
replace uniqueid=30137 if municipality =="LOS REYES"
replace uniqueid=30098 if municipality =="MAGDALENA"
replace uniqueid=30099 if municipality =="MALTRATA"
replace uniqueid=30100 if municipality =="MANLIO FABIO ALTAMIRANO"
replace uniqueid=30101 if municipality =="MARIANO ESCOBEDO"
replace uniqueid=30102 if municipality =="MARTINEZ DE LA TORRE"
replace uniqueid=30103 if municipality =="MECATLAN"
replace uniqueid=30104 if municipality =="MECAYAPAN"
replace uniqueid=30105 if municipality =="MEDELLIN"
replace uniqueid=30106 if municipality =="MIAHUATLAN"
replace uniqueid=30108 if municipality =="MINATITLAN"
replace uniqueid=30109 if municipality =="MISANTLA"
replace uniqueid=30110 if municipality =="MIXTLA DE ALTAMIRANO"
replace uniqueid=30111 if municipality =="MOLOACAN"
replace uniqueid=30206 if municipality =="NANCHITAL DE L.C. DEL RIO"
replace uniqueid=30112 if municipality =="NAOLINCO"
replace uniqueid=30113 if municipality =="NARANJAL"
replace uniqueid=30013 if municipality =="NARANJOS AMATLAN"
replace uniqueid=30114 if municipality =="NAUTLA"
replace uniqueid=30115 if municipality =="NOGALES"
replace uniqueid=30116 if municipality =="OLUTA"
replace uniqueid=30117 if municipality =="OMEALCA"
replace uniqueid=30118 if municipality =="ORIZABA"
replace uniqueid=30119 if municipality =="OTATITLAN"
replace uniqueid=30120 if municipality =="OTEAPAN"
replace uniqueid=30121 if municipality =="OZULUAMA"
replace uniqueid=30122 if municipality =="PAJAPAN"
replace uniqueid=30123 if municipality =="PANUCO"
replace uniqueid=30124 if municipality =="PAPANTLA"
replace uniqueid=30126 if municipality =="PASO DE OVEJAS"
replace uniqueid=30125 if municipality =="PASO DEL MACHO"
replace uniqueid=30128 if municipality =="PEROTE"
replace uniqueid=30129 if municipality =="PLATON SANCHEZ"
replace uniqueid=30130 if municipality =="PLAYA VICENTE"
replace uniqueid=30131 if municipality =="POZA RICA"
replace uniqueid=30133 if municipality =="PUEBLO VIEJO"
replace uniqueid=30134 if municipality =="PUENTE NACIONAL"
replace uniqueid=30135 if municipality =="RAFAEL DELGADO"
replace uniqueid=30136 if municipality =="RAFAEL LUCIO"
replace uniqueid=30138 if municipality =="RIO BLANCO"
replace uniqueid=30139 if municipality =="SALTABARRANCA"
replace uniqueid=30140 if municipality =="SAN ANDRES TENEJAPAN"
replace uniqueid=30141 if municipality =="SAN ANDRES TUXTLA"
replace uniqueid=30142 if municipality =="SAN JUAN EVANGELISTA"
replace uniqueid=30143 if municipality =="SANTIAGO TUXTLA"
replace uniqueid=30144 if municipality =="SAYULA DE ALEMAN"
replace uniqueid=30146 if municipality =="SOCHIAPA"
replace uniqueid=30145 if municipality =="SOCONUSCO"
replace uniqueid=30147 if municipality =="SOLEDAD ATZOMPA"
replace uniqueid=30148 if municipality =="SOLEDAD DE DOBLADO"
replace uniqueid=30149 if municipality =="SOTEAPAN"
replace uniqueid=30150 if municipality =="TAMALIN"
replace uniqueid=30151 if municipality =="TAMIAHUA"
replace uniqueid=30152 if municipality =="TAMPICO ALTO"
replace uniqueid=30153 if municipality =="TANCOCO"
replace uniqueid=30154 if municipality =="TANTIMA"
replace uniqueid=30155 if municipality =="TANTOYUCA"
replace uniqueid=30209 if municipality =="TATAHUICAPAN"
replace uniqueid=30156 if municipality =="TATATILA"
replace uniqueid=30158 if municipality =="TECOLUTLA"
replace uniqueid=30159 if municipality =="TEHUIPANGO"
replace uniqueid=30161 if municipality =="TEMPOAL"
replace uniqueid=30162 if municipality =="TENAMPA"
replace uniqueid=30163 if municipality =="TENOCHTITLAN"
replace uniqueid=30164 if municipality =="TEOCELO"
replace uniqueid=30165 if municipality =="TEPATLAXCO"
replace uniqueid=30166 if municipality =="TEPETLAN"
replace uniqueid=30167 if municipality =="TEPETZINTLA"
replace uniqueid=30168 if municipality =="TEQUILA"
replace uniqueid=30170 if municipality =="TEXCATEPEC"
replace uniqueid=30171 if municipality =="TEXHUACAN"
replace uniqueid=30172 if municipality =="TEXISTEPEC"
replace uniqueid=30173 if municipality =="TEZONAPA"
replace uniqueid=30174 if municipality =="TIERRA BLANCA"
replace uniqueid=30175 if municipality =="TIHUATLAN"
replace uniqueid=30180 if municipality =="TLACHICHILCO"
replace uniqueid=30176 if municipality =="TLACOJALPAN"
replace uniqueid=30177 if municipality =="TLACOLULAN"
replace uniqueid=30178 if municipality =="TLACOTALPAN"
replace uniqueid=30179 if municipality =="TLACOTEPEC DE MEJIA"
replace uniqueid=30181 if municipality =="TLALIXCOYAN"
replace uniqueid=30182 if municipality =="TLALNELHUAYOCAN"
replace uniqueid=30024 if municipality =="TLALTETELA"
replace uniqueid=30183 if municipality =="TLAPACOYAN"
replace uniqueid=30184 if municipality =="TLAQUILPAN"
replace uniqueid=30185 if municipality =="TLILAPAN"
replace uniqueid=30186 if municipality =="TOMATLAN"
replace uniqueid=30187 if municipality =="TONAYAN"
replace uniqueid=30188 if municipality =="TOTUTLA"
replace uniqueid=30207 if municipality =="TRES VALLES"
replace uniqueid=30189 if municipality =="TUXPAN"
replace uniqueid=30190 if municipality =="TUXTILLA"
replace uniqueid=30191 if municipality =="URSULO GALVAN"
replace uniqueid=30210 if municipality =="UXPANAPA"
replace uniqueid=30192 if municipality =="VEGA DE ALATORRE"
replace uniqueid=30193 if municipality =="VERACRUZ"
replace uniqueid=30194 if municipality =="VILLA ALDAMA"
replace uniqueid=30087 if municipality =="XALAPA"
replace uniqueid=30092 if municipality =="XICO"
replace uniqueid=30195 if municipality =="XOXOCOTLA"
replace uniqueid=30196 if municipality =="YANGA"
replace uniqueid=30197 if municipality =="YECUATLA"
replace uniqueid=30198 if municipality =="ZACUALPAN"
replace uniqueid=30199 if municipality =="ZARAGOZA"
replace uniqueid=30200 if municipality =="ZENTLA"
replace uniqueid=30201 if municipality =="ZONGOLICA"
replace uniqueid=30202 if municipality =="ZONTECOMATLAN"
replace uniqueid=30203 if municipality =="ZOZOCOLCO DE HIDALGO"

egen valid = rowtotal(PAN PRI PRD PT PVEM PC PCD PDS PAN_PRD PC_PCD PC_PSN PAN_PC PAN_PDS PAN_PRD_PC PAN_PRD_PVEM PAN_PT PAN_PVEM PARM_PDS PARM_PDS_PAS PARM_PAS PAS_PC PAS_PVEM PRD_PC PRD_PC_PSN_PARM PRD_PARM_PCD PRD_PT PRD_PT_PC PRD_PT_PC_PAS PRD_PT_PVEM_PC PRD_PT_PVEM_PCD PRD_PVEM PRD_PVEM_PC_PAS PRI_PDS PRI_PARM PRI_PARM_PDS PRI_PARM_PDS_PAS PRI_PARM_PAS PRI_PARM_PSN PRI_PARM_PSN_PDS PRI_PARM_PSN_PDS_PAS PRI_PARM_PSN_PAS PRI_PAS PRI_PSN PRI_PSN_PDS PRI_PSN_PDS_PAS PRI_PSN_PAS PT_PC PT_PC_PARM PT_PC_PSN PT_PDS PT_PARM PT_PVEM PT_PVEM_PC PT_PVEM_PC_PAS_PDS PT_PVEM_PARM_PDS PT_PVEM_PAS PVEM_PC PVEM_PARM_PC PVEM_PSN)

foreach var in PAN PRI PRD PT PVEM PC PCD PDS PAN_PRD PC_PCD PC_PSN PAN_PC PAN_PDS PAN_PRD_PC PAN_PRD_PVEM PAN_PT PAN_PVEM PARM_PDS PARM_PDS_PAS PARM_PAS PAS_PC PAS_PVEM PRD_PC PRD_PC_PSN_PARM PRD_PARM_PCD PRD_PT PRD_PT_PC PRD_PT_PC_PAS PRD_PT_PVEM_PC PRD_PT_PVEM_PCD PRD_PVEM PRD_PVEM_PC_PAS PRI_PDS PRI_PARM PRI_PARM_PDS PRI_PARM_PDS_PAS PRI_PARM_PAS PRI_PARM_PSN PRI_PARM_PSN_PDS PRI_PARM_PSN_PDS_PAS PRI_PARM_PSN_PAS PRI_PAS PRI_PSN PRI_PSN_PDS PRI_PSN_PDS_PAS PRI_PSN_PAS PT_PC PT_PC_PARM PT_PC_PSN PT_PDS PT_PARM PT_PVEM PT_PVEM_PC PT_PVEM_PC_PAS_PDS PT_PVEM_PARM_PDS PT_PVEM_PAS PVEM_PC PVEM_PARM_PC PVEM_PSN total valid{
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

g ed = 30
g seccion = section
capture merge 1:m ed seccion using "..\..\all_months_years.dta", keepusing(month year lista)
keep if month==9 & year==2000
drop if _merge==2
drop _merge ed seccion year month
rename lista listanominal

g turnout = total/listanominal

foreach var in listanominal {
bys uniqueid: egen mun_`var'= sum(`var') 
gen inv_mun_`var'= 1/mun_`var'
}

gen mun_turnout =  mun_total/mun_listanominal

rowranks inv_mun_PAN inv_mun_PRI inv_mun_PRD inv_mun_PT inv_mun_PVEM inv_mun_PC inv_mun_PCD inv_mun_PDS inv_mun_PAN_PRD inv_mun_PC_PCD inv_mun_PC_PSN inv_mun_PAN_PC inv_mun_PAN_PDS inv_mun_PAN_PRD_PC inv_mun_PAN_PRD_PVEM inv_mun_PAN_PT inv_mun_PAN_PVEM inv_mun_PARM_PDS inv_mun_PARM_PDS_PAS inv_mun_PARM_PAS inv_mun_PAS_PC inv_mun_PAS_PVEM inv_mun_PRD_PC inv_mun_PRD_PC_PSN_PARM inv_mun_PRD_PARM_PCD inv_mun_PRD_PT inv_mun_PRD_PT_PC inv_mun_PRD_PT_PC_PAS inv_mun_PRD_PT_PVEM_PC inv_mun_PRD_PT_PVEM_PCD inv_mun_PRD_PVEM inv_mun_PRD_PVEM_PC_PAS inv_mun_PRI_PDS inv_mun_PRI_PARM inv_mun_PRI_PARM_PDS inv_mun_PRI_PARM_PDS_PAS inv_mun_PRI_PARM_PAS inv_mun_PRI_PARM_PSN inv_mun_PRI_PARM_PSN_PDS inv_mun_PRI_PARM_PSN_PDS_PAS inv_mun_PRI_PARM_PSN_PAS inv_mun_PRI_PAS inv_mun_PRI_PSN inv_mun_PRI_PSN_PDS inv_mun_PRI_PSN_PDS_PAS inv_mun_PRI_PSN_PAS inv_mun_PT_PC inv_mun_PT_PC_PARM inv_mun_PT_PC_PSN inv_mun_PT_PDS inv_mun_PT_PARM inv_mun_PT_PVEM inv_mun_PT_PVEM_PC inv_mun_PT_PVEM_PC_PAS_PDS inv_mun_PT_PVEM_PARM_PDS inv_mun_PT_PVEM_PAS inv_mun_PVEM_PC inv_mun_PVEM_PARM_PC inv_mun_PVEM_PSN, gen(PAN_r PRI_r PRD_r PT_r PVEM_r PC_r PCD_r PDS_r PAN_PRD_r PC_PCD_r PC_PSN_r PAN_PC_r PAN_PDS_r PAN_PRD_PC_r PAN_PRD_PVEM_r PAN_PT_r PAN_PVEM_r PARM_PDS_r PARM_PDS_PAS_r PARM_PAS_r PAS_PC_r PAS_PVEM_r PRD_PC_r PRD_PC_PSN_PARM_r PRD_PARM_PCD_r PRD_PT_r PRD_PT_PC_r PRD_PT_PC_PAS_r PRD_PT_PVEM_PC_r PRD_PT_PVEM_PCD_r PRD_PVEM_r PRD_PVEM_PC_PAS_r PRI_PDS_r PRI_PARM_r PRI_PARM_PDS_r PRI_PARM_PDS_PAS_r PRI_PARM_PAS_r PRI_PARM_PSN_r PRI_PARM_PSN_PDS_r PRI_PARM_PSN_PDS_PAS_r PRI_PARM_PSN_PAS_r PRI_PAS_r PRI_PSN_r PRI_PSN_PDS_r PRI_PSN_PDS_PAS_r PRI_PSN_PAS_r PT_PC_r PT_PC_PARM_r PT_PC_PSN_r PT_PDS_r PT_PARM_r PT_PVEM_r PT_PVEM_PC_r PT_PVEM_PC_PAS_PDS_r PT_PVEM_PARM_PDS_r PT_PVEM_PAS_r PVEM_PC_r PVEM_PARM_PC_r PVEM_PSN_r)
drop inv_mun_*

gen winner = "" 
foreach var in PAN PRI PRD PT PVEM PC PCD PDS PAN_PRD PC_PCD PC_PSN PAN_PC PAN_PDS PAN_PRD_PC PAN_PRD_PVEM PAN_PT PAN_PVEM PARM_PDS PARM_PDS_PAS PARM_PAS PAS_PC PAS_PVEM PRD_PC PRD_PC_PSN_PARM PRD_PARM_PCD PRD_PT PRD_PT_PC PRD_PT_PC_PAS PRD_PT_PVEM_PC PRD_PT_PVEM_PCD PRD_PVEM PRD_PVEM_PC_PAS PRI_PDS PRI_PARM PRI_PARM_PDS PRI_PARM_PDS_PAS PRI_PARM_PAS PRI_PARM_PSN PRI_PARM_PSN_PDS PRI_PARM_PSN_PDS_PAS PRI_PARM_PSN_PAS PRI_PAS PRI_PSN PRI_PSN_PDS PRI_PSN_PDS_PAS PRI_PSN_PAS PT_PC PT_PC_PARM PT_PC_PSN PT_PDS PT_PARM PT_PVEM PT_PVEM_PC PT_PVEM_PC_PAS_PDS PT_PVEM_PARM_PDS PT_PVEM_PAS PVEM_PC PVEM_PARM_PC PVEM_PSN {
replace winner = "`var'" if `var'_r ==1 
}
drop *_r

gen year = 2000
gen month ="September"

save Veracruz_Section_2000.dta, replace

**************************************************************************
**************************************************************************
**************************************************************************
