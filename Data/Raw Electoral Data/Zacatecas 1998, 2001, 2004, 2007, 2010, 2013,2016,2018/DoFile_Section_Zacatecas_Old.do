
gen section_merging = seccion
replace  inegi_code = 32017 if inegi_code== 32057 & year==1998 & (section_merging== 516 | (section_merging>= 525 &  section_merging<= 533) | section_merging== 548)
replace  inegi_code = 32047 if inegi_code== 32058 & year>=1998 & year<=2004 & ((section_merging>= 1469 &  section_merging<= 1474) | (section_merging>= 1482 &  section_merging<= 1483))

* all _merge==2 are "BAJA POR INTEGRACION SECCIONAL 2008"
* 72	BAJA POR INTEGRACION SECCIONAL 2008
* 905	BAJA POR INTEGRACION SECCIONAL 2008
* 1565	BAJA POR INTEGRACION SECCIONAL 2008
* 1595	BAJA POR INTEGRACION SECCIONAL 2008
* 1768	BAJA POR INTEGRACION SECCIONAL 2008
* 1771	BAJA POR INTEGRACION SECCIONAL 2008

