

gen    section_merging = section 
replace section_merging = 2604 if section_merging >= 3327 & section_merging <= 3355 &  year ==2006

tab _merge
* 119, 122, 134 to 137, 2385 to 2387 is newly created municipality (San Ignacio Cerro Gordo) w/repeated section
* 1548, 2484 are newly created municipality (Tlaquepaque) w/repeated section
* 2024, 3338 are newly created municipality (Tonala) w/repeated section
* 2330, 2462 are outliers
* 49	BAJA POR INTEGRACION SECCIONAL 2008
* 234	BAJA POR INTEGRACION SECCIONAL 2008
* 934	BAJA POR INTEGRACION SECCIONAL 2008
* 2376	BAJA POR INTEGRACION SECCIONAL 2008
