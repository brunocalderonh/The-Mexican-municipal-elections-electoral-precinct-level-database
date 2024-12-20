

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

gen    section_merging = section 
gen    uniqueid_merging = uniqueid

replace uniqueid_merging = 12076 if uniqueid == 12072 & year==2005 & section_merging == 2714
replace uniqueid_merging = 12013 if uniqueid == 12077 & year==2005 & (section_merging ==732 | section_merging ==735 | section_merging ==747 | (section_merging >=752 & section_merging <=756) | section_merging ==759 | section_merging ==762)

replace uniqueid_merging = 12013 if uniqueid == 12080 & year==2008 & (section_merging == 739 | (section_merging >=743 & section_merging <=746) | (section_merging >=748 & section_merging <=750) | section_merging ==757 ) 
replace uniqueid_merging = 12013 if uniqueid == 12077 & year==2008 & section_merging == 758
replace uniqueid_merging = 12041 if uniqueid == 12081 & year==2008 & (section_merging == 1709 | (section_merging >=1712 & section_merging <=1714))
replace uniqueid_merging = 12043 if uniqueid == 12078 & year==2008 & (section_merging == 1743 | (section_merging >=1747 & section_merging <=1748) | (section_merging >=1750 & section_merging <=1751) | (section_merging >=1756 & section_merging <=1761)) 
replace uniqueid_merging = 12052 if uniqueid == 12081 & year==2008 & section_merging == 2019


**************************************************************************
**************************************************************************
**************************************************************************


* Marquelia was carved from Azoyu
replace inegi_code = 12077 if  inegi_code ==12013 & year>=2005 & year<=2008 & (section_merging== 732 | section_merging== 735 | section_merging==  747 | (section_merging>=752 & section_merging<=756) | section_merging== 759 | section_merging== 762 | section_merging==  1018)
replace inegi_code = 12077 if  inegi_code ==12013 & year==2008 & section_merging== 758 
* Creation of Juchitan municipality
replace inegi_code = 12080 if  inegi_code ==12013 & year==2008 & (section_merging== 739  | (section_merging>=743 & section_merging<=746) | (section_merging>=748 & section_merging<=750))
* Jose Joaquin de Herrera was previously Chilapa de Alvarez
replace inegi_code = 12028 if  inegi_code ==12079 & year>=1996 & year<=2002 & (section_merging== 1185  | (section_merging>=1190 & section_merging<=1192) | (section_merging>=1195 & section_merging<=1196) | (section_merging>=1198 & section_merging<=1199))
* Iliatenco split from Malinaltepec 
replace inegi_code = 12041 if  inegi_code ==12081 & year>=1996 & year<=2005 & (section_merging== 1709 |  (section_merging>=1712 & section_merging<=1714))
* Cochoapa El Grande split from Metlatonoc
replace inegi_code = 12078 if  inegi_code ==12043 & year==2008 & (section_merging== 1743  | (section_merging>=1747 & section_merging<=1748) | (section_merging>=1750 & section_merging<=1751) | (section_merging>=1756 & section_merging<=1759) | section_merging== 1761 )
* San Luis Acatan was carved from Iliatenco
replace inegi_code = 12052 if inegi_code == 12081 & year>=1996 & year<=2005 & section_merging == 2019
* Marquelia was carved from Cuajincuilapa 
replace inegi_code = 12077 if inegi_code == 12023 & year>=2005 & year<=2008& section_merging == 1018

* 526	BAJA POR INTEGRACION SECCIONAL 2008
* 648	BAJA POR INTEGRACION SECCIONAL 2008
* 757	BAJA POR INTEGRACION SECCIONAL 2008
* 1078	BAJA POR INTEGRACION SECCIONAL 2008
* 1272	BAJA POR INTEGRACION SECCIONAL 2008
* 1343	BAJA POR INTEGRACION SECCIONAL 2008
* 1368	BAJA POR INTEGRACION SECCIONAL 2008
* 1400	BAJA POR INTEGRACION SECCIONAL 2008
* 1779	BAJA POR INTEGRACION SECCIONAL 2008
* 1782	BAJA POR INTEGRACION SECCIONAL 2008
* 2081	BAJA POR INTEGRACION SECCIONAL 2008
* 2282	BAJA POR INTEGRACION SECCIONAL 2008
* 2357	BAJA POR INTEGRACION SECCIONAL 2008
* 2395	BAJA POR INTEGRACION SECCIONAL 2008
* 2498	BAJA POR INTEGRACION SECCIONAL 2008
* 2704	BAJA POR INTEGRACION SECCIONAL 2008
* 2722	BAJA POR INTEGRACION SECCIONAL 2008
* 2527, 2714, 2778 - 2782 belong to municipality ACATEPEC, which is newly a created municipality
* 9980 to 9994 are clear outliers
