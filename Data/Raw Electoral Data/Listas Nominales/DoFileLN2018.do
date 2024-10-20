

clear 

cd "C:\Users\Salvador\Dropbox\Salvador Ascencio\Update Precincts\Listas Nominales"


import excel "Diputaciones_PREP_ListaNominal.xlsx", sheet("diputaciones") clear firstrow allstring
rename ESTADO STATE
replace STATE=subinstr(STATE, "Á", "A", . )
replace STATE=subinstr(STATE, "É", "E", . )
replace STATE=subinstr(STATE, "Í", "I", . )
replace STATE=subinstr(STATE, "Ó", "O", . )

destring *, replace
collapse (sum) LISTA_NOMINAL, by(STATE SECCION)
rename LISTA_NOMINAL LISTA_NOMINAL_DIP

save Diputaciones_PREP_ListaNominal.dta, replace

import excel "Presidencia_PREP_ListaNominal.xlsx", sheet("presidencia") clear firstrow allstring
rename ESTADO STATE
replace STATE=subinstr(STATE, "Á", "A", . )
replace STATE=subinstr(STATE, "É", "E", . )
replace STATE=subinstr(STATE, "Í", "I", . )
replace STATE=subinstr(STATE, "Ó", "O", . )


destring *, replace
collapse (sum) LISTA_NOMINAL, by(STATE SECCION)

rename LISTA_NOMINAL LISTA_NOMINAL_PRES

save Presidencia_PREP_ListaNominal.dta, replace

import excel "Senadurias_PREP_ListaNominal.xlsx", sheet("senadurias") clear firstrow allstring
rename ESTADO STATE
replace STATE=subinstr(STATE, "Á", "A", . )
replace STATE=subinstr(STATE, "É", "E", . )
replace STATE=subinstr(STATE, "Í", "I", . )
replace STATE=subinstr(STATE, "Ó", "O", . )


destring *, replace
collapse (sum) LISTA_NOMINAL, by(STATE SECCION)

rename LISTA_NOMINAL LISTA_NOMINAL_SEN

save Senadurias_PREP_ListaNominal.dta, replace

use Presidencia_PREP_ListaNominal.dta, clear
merge 1:1 STATE SECCION using Diputaciones_PREP_ListaNominal.dta
drop _merge
merge 1:1 STATE SECCION using Senadurias_PREP_ListaNominal.dta
drop _merge
drop if SECCION==.
corr LISTA_NOMINAL_PRES LISTA_NOMINAL_DIP LISTA_NOMINAL_SEN
keep STATE SECCION LISTA_NOMINAL_PRES
rename LISTA_NOMINAL_PRES ListadoNominalINE
rename SECCION section

save ListadoNominalPREP2018.dta, replace

erase Presidencia_PREP_ListaNominal.dta
erase Senadurias_PREP_ListaNominal.dta
erase Diputaciones_PREP_ListaNominal.dta
