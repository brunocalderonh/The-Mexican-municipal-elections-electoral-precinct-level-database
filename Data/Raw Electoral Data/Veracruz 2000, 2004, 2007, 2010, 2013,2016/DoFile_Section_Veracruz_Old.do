
gen section_merging = seccion
replace section_merging = 816 if section_merging >= 4723 & section_merging <= 4745 &  year <=2007 
replace section_merging = 4382 if section_merging >= 4746 & section_merging <= 4757 &  year <=2007
replace inegi_code = 30102 if  inegi_code == 30211 & year==2000 & ((section_merging>= 2319 & section_merging<= 2326) | (section_merging>= 2333 & section_merging<= 2346) | (section_merging>= 2351 & section_merging<= 2357))
replace inegi_code = 30130 if  inegi_code == 30212 & year==2000 & ((section_merging>= 3100 & section_merging<= 3101) | (section_merging>= 3107 & section_merging<= 3108))
replace inegi_code = 30146 if  inegi_code == 30212 & year==2007 & ((section_merging>= 3100 & section_merging<= 3101) | (section_merging>= 3107 & section_merging<= 3108) | (section_merging>= 4720 & section_merging<= 4722))

* almmost all _merge==2 are "BAJA POR INTEGRACION SECCIONAL 2008"
* 866	BAJA POR INTEGRACION SECCIONAL 2008
* 966	BAJA POR INTEGRACION SECCIONAL 2008
* 2324	BAJA POR INTEGRACION SECCIONAL 2008
* 2330	BAJA POR INTEGRACION SECCIONAL 2008
* + 1623 to 1640 in 2010
drop if _merge!=3
