


replace section_merging = 1141 if section_merging >= 5938 & section_merging <= 5953 &  year ==2003
replace section_merging = 1269 if section_merging >= 5954 & section_merging <= 5970 &  year ==2003
replace section_merging = 1277 if section_merging >= 5971 & section_merging <= 5990 &  year ==2003
replace section_merging = 1304 if section_merging >= 5991 & section_merging <= 6008 &  year ==2003
replace section_merging = 1322 if section_merging >= 6009 & section_merging <= 6027 &  year ==2003
replace section_merging = 1781 if section_merging >= 6028 & section_merging <= 6049 &  year ==2003
replace section_merging = 2081 if section_merging >= 6050 & section_merging <= 6064 &  year ==2003
replace section_merging = 2096 if section_merging >= 6065 & section_merging <= 6170 &  year ==2003
replace section_merging = 5648 if section_merging >= 6171 & section_merging <= 6182 &  year ==2003

replace inegi_code = 15031 if inegi_code ==15058 & ((section_merging >= 1253 & section_merging <= 1254) | (section_merging >= 1258 & section_merging <= 1265)) 
replace inegi_code = 15044 if inegi_code ==15125 & section_merging >= 2247 & section_merging <= 2249 & year >=1996  & year <=2000
replace inegi_code = 15074 if inegi_code ==15124 & ((section_merging >= 4027 & section_merging <= 4033) | (section_merging >= 4044 & section_merging <= 4050) | (section_merging >= 4062 & section_merging <= 4067) | (section_merging >= 4080 & section_merging <= 4085) | (section_merging >= 4091 & section_merging <= 4096) | (section_merging >= 4098 & section_merging <= 4102) | (section_merging >= 4104 & section_merging <= 4105) ) & year >=1996  & year <=2000
replace inegi_code = 15082 if inegi_code ==15123 & ((section_merging >= 4266  & section_merging <= 4273) | (section_merging == 4278) | (section_merging >= 4281 & section_merging <= 4282) | (section_merging >= 4284 & section_merging <= 4290) | (section_merging == 4296) | (section_merging >= 4301 & section_merging <= 4302) | (section_merging == 4306) | (section_merging >= 4317 & section_merging <= 4318 ) | (section_merging == 4320) ) & year >=1996  & year <=2000

* All _merge==2 are "BAJA POR INTEGRACION SECCIONAL"
* 488	BAJA POR INTEGRACION SECCIONAL 2008
* 1731	BAJA POR INTEGRACION SECCIONAL 2008
* 4120	BAJA POR INTEGRACION SECCIONAL 2008
* 4311	BAJA POR INTEGRACION SECCIONAL 2008
* 4699	BAJA POR INTEGRACION SECCIONAL 2008

