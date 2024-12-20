

**************************************************************************
**************************************************************************
**************************************************************************

gen    section_merging = section 
replace section_merging = 1022 if section_merging >= 1740 & section_merging <= 1775 &  year ==2010
replace section_merging = 1023 if section_merging >= 1776 & section_merging <= 1791 &  year ==2010
replace section_merging = 1092 if section_merging >= 1792 & section_merging <= 1817 &  year ==2010

* 7	    BAJA POR INTEGRACION SECCIONAL 2008
* 176	BAJA POR INTEGRACION SECCIONAL 2008
* 331	BAJA POR INTEGRACION SECCIONAL 2008
* 1357	BAJA POR INTEGRACION SECCIONAL 2008
* + 299, 370, 1108, 1414 which are outliers because they are repeated
