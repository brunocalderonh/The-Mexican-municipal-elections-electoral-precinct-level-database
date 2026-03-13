
cd "C:\Users\Horacio\Dropbox\Audits\Election Data\Colima - 2003, 2006, 2009, 2012\CASILLAS_AYUNTAMIENTOS_2012"

import excel "Armeria.xlsx", sheet("Sheet1") firstrow clear

gen municipality="ARMERIA"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

save Armeria.dta, replace

************************************************************************

import excel "Colima.xlsx", sheet("Sheet1") firstrow clear

gen municipality="COLIMA"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2+Casillas3
drop Casillas*
destring *, replace

save Colima.dta, replace

************************************************************************

import excel "Comala.xlsx", sheet("Sheet1") firstrow clear

gen municipality="COMALA"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

save Comala.dta, replace

************************************************************************

import excel "Coquimatlan.xlsx", sheet("Sheet1") firstrow clear

gen municipality="COQUIMATLAN"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

save Coquimatlan.dta, replace

************************************************************************

import excel "Cuauhtemoc.xlsx", sheet("Sheet1") firstrow clear

gen municipality="CUAUHTEMOC"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

drop K-S
save Cuauhtemoc.dta, replace

************************************************************************

import excel "Ixtlahuacan.xlsx", sheet("Sheet1") firstrow clear

gen municipality="IXTLAHUACAN"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

drop K-S
save Ixtlahuacan.dta, replace

************************************************************************

import excel "Ixtlahuacan.xlsx", sheet("Sheet1") firstrow clear

gen municipality="COLIMA"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

drop K-S
save Ixtlahuacan.dta, replace

************************************************************************

import excel "Manzanillo.xlsx", sheet("Sheet1") firstrow clear

gen municipality="MANZANILLO"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2+Casillas3
drop Casillas*
destring *, replace

drop K
save Manzanillo.dta, replace

************************************************************************

import excel "Minatitlan.xlsx", sheet("Sheet1") firstrow clear

gen municipality="MINATITLAN"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2
drop Casillas*
destring *, replace

drop K-S
save Minatitlan.dta, replace

************************************************************************

import excel "Tecoman.xlsx", sheet("Sheet1") firstrow clear

gen municipality="TECOMAN"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2+Casillas3
drop Casillas*
destring *, replace

drop K
save Tecoman.dta, replace

************************************************************************


import excel "V DE A.xlsx", sheet("Sheet1") firstrow clear

gen municipality="VILLA DE ALVAREZ"
rename PRIAlianca PRI_PANAL
rename Verde PVEM
rename MovimientoCiudadano PC

replace Casillas= trim(Casillas)
drop if strpos(Casillas, "TOTAL")>0
drop if strpos(Casillas, "PORCENTAJE")>0
drop if Casillas ==""

split Casillas, p(" ")
rename Casillas1 section
gen type = Casillas2+Casillas3
drop Casillas*
destring *, replace

save VillaDeAlvarez.dta, replace

************************************************************************

clear
append using Armeria.dta
append using Colima.dta
append using Comala.dta
append using Coquimatlan.dta
append using Cuauhtemoc.dta
append using Ixtlahuacan.dta
append using Manzanillo.dta
append using Minatitlan.dta
append using Tecoman.dta
append using VillaDeAlvarez.dta

save Ayu_Seccion_2012.dta, replace






