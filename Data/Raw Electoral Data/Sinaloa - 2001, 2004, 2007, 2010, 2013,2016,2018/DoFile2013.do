

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Sinaloa - 2001, 2004, 2007, 2010, 2013\LISTA NOMINAL Sinaloa  2013"

local i = 0
foreach case in "01 Choix Último" "02 El Fuerte Último" "03 Ahome Último" "04 Ahome Último" "05 Sinaloa Último" "06 Guasave Último" "07 Guasave Último" "08 Angostura Último" "09 Salvador Último" "10 Mocorito Último" "11 Badiraguato Último" "12 Culiacán Último" "13 Culiacán Último" "14 Culiacán Último" "15 Navolato Último" "16 Cosalá Último" "17 Elota Último" "18 San Ignacio Último" "19 Mazatlán Último" "20 Mazatlán Último" "21 Concordia Último" "22 Rosario Último" "23 Escuinapa Último" "24 Culiacán Último" {
local i = `i' + 1
import excel "`case'.xlsx", sheet("Hoja1") clear
save case_`i'.dta, replace
display `i'
}

clear
forvalues i=1(1)24 {
append using case_`i'.dta
erase case_`i'.dta
}


drop if A=="Corte Definitivo de Lista Nominal Sinaloa 2013" | A=="DISTRITO"
drop if A==""

duplicates tag, generate(dup)
drop if dup>0
drop dup

replace F = E if F==""
drop E

destring *, replace

rename B municipality
rename C section
rename F listanominal

collapse (sum) listanominal, by(municipality section)
bys sect: gen index=_n
drop municipality index

sort section 

cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Sinaloa - 2001, 2004, 2007, 2010, 2013"

save listadonominal2013.dta, replace

******************************************************************************************
******************************************************************************************
******************************************************************************************

capture cd "C:\Users\Horacio\Dropbox\Incumbency Advantage\Precinct\Sinaloa - 2001, 2004, 2007, 2010, 2013"

local i = 0
foreach muni in "CHOIX" "EL FUERTE" "AHOME" "SINALOA" "GUASAVE" "ANGOSTURA" "SALVADOR ALVARADO" "MOCORITO" "BADIRAGUATO" "CULIACAN" "NAVOLATO" "COSALA" "ELOTA" "SAN IGNACIO" "MAZATLAN" "CONCORDIA" "ROSARIO" "ESCUINAPA" {
local i = `i' + 1
import excel "Ayu_Seccion_2013.xlsx", sheet("`muni'") firstrow clear
gen municipality ="`muni'"
save case_`i'.dta, replace 
}


clear
forvalues i=1(1)18 {
append using case_`i'.dta
erase case_`i'.dta
}

drop G
rename MC PC

rename Casila section
drop if section == ""

drop if strpos(section, "ES")>0
replace section = subinstr(section, "B", "",.)
replace section = subinstr(section, " ", "",.)
foreach i in 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 {
replace section = subinstr(section, "C`i'", "",.)
replace section = subinstr(section, "EX`i'", "",.)
}

destring *, replace

collapse (sum) PAN_PRD_PT PRI_PVEM_PANAL PAS PC PRI_PVEM_PANAL_PAS NoRegistrados Nulos, by(municipality section)

egen total = rowtotal(PAN_PRD_PT PRI_PVEM_PANAL PAS PC PRI_PVEM_PANAL_PAS NoRegistrados Nulos)
egen valid = rowtotal(PAN_PRD_PT PRI_PVEM_PANAL PAS PC PRI_PVEM_PANAL_PAS)

sort section 
merge section using listadonominal2013.dta
drop if _merge==2
drop _merge

save Ayu_Seccion_2013.dta, replace

